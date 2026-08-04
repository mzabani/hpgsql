{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{- FOURMOLU_DISABLE -} -- CPP macros make fourmolu fail

module Hpgsql.GhcParseExp (parseExp, canParseExp) where

import Data.Char (isUpper)
import Data.Either (isRight)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import GHC.Hs (GhcPs)
import GHC.Parser (parseExpression)
import GHC.Parser.Lexer (P (..), ParseResult (..), initParserState)
import GHC.Parser.PostProcess (ECP (..), runPV)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SourceText (IntegralLit (..), rationalFromFractionalLit)
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc)
import Hpgsql.GhcParserOpts (fakeSettings)
import Hpgsql.LanguageHaskell.FromThExtension (fromThToGhcLibExtension)
import Language.Haskell.Syntax (FieldOcc (..), GRHS (..), GRHSs (..), HsConDetails (..), HsConPatDetails, HsFieldBind (..), HsLit (..), HsLocalBinds, HsLocalBindsLR (..), HsOverLit (..), HsRecFields (..), HsSigType (..), HsTupArg (..), HsType (..), HsWildCardBndrs (..), LHsExpr, LHsRecField, LHsSigWcType, LMatch, LPat, Match (..), MatchGroup (..), OverLitVal (..), Pat (..), PromotionFlag (..))
import Language.Haskell.Syntax.Basic (FieldLabelString (..))
import Language.Haskell.Syntax.Expr (DotFieldOcc (..), HsExpr (..))
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import qualified "template-haskell" Language.Haskell.TH as TH

-- TODO: How about source locations/lines? Do we need them?

-- | Parse a Haskell expression string into a Template Haskell Exp.
parseExp :: [TH.Extension] -> String -> Either String TH.Exp
parseExp callerExtensions str = do
  hsExpr <- ghcParse callerExtensions str
  convertExpr hsExpr

-- | Check if a string can be parsed as a Haskell expression.
canParseExp :: [TH.Extension] -> String -> Bool
canParseExp callerExtensions = isRight . ghcParse callerExtensions

ghcParse :: [TH.Extension] -> String -> Either String (HsExpr GhcPs)
ghcParse callerExtensions str =
  let buf = stringToStringBuffer str
      loc = mkRealSrcLoc (mkFastString "<hpgsql>") 1 1
      opts = initParserOpts parserDynFlags
      parseExprP = parseExpression >>= \ecp -> runPV (unECP ecp)
   in case unP parseExprP (initParserState opts buf loc) of
        POk _ (L _ expr) -> Right expr
        PFailed _ -> Left "Failed to parse Haskell expression"
  where
    parserDynFlags :: DynFlags
    parserDynFlags =
      List.foldl'
        xopt_set
        (defaultDynFlags fakeSettings)
        (mapMaybe fromThToGhcLibExtension callerExtensions)

--
-- GHC HsExpr to TH Exp conversion

convertExpr :: HsExpr GhcPs -> Either String TH.Exp
convertExpr (HsVar _ (L _ rdr)) = Right (rdrToExp rdr)
convertExpr (HsApp _ (L _ f) (L _ x)) = TH.AppE <$> convertExpr f <*> convertExpr x
convertExpr (OpApp _ (L _ l) (L _ op) (L _ r)) = do
  l' <- convertExpr l
  op' <- convertExpr op
  r' <- convertExpr r
  Right (TH.UInfixE l' op' r')
convertExpr (NegApp _ (L _ e) _) = do
  e' <- convertExpr e
  Right $ TH.AppE (TH.VarE 'negate) e'

#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertExpr (HsPar _ (L _ e)) = TH.ParensE <$> convertExpr e
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
convertExpr (HsPar _ _ (L _ e) _) = TH.ParensE <$> convertExpr e
#endif
convertExpr (ExplicitList _ es) = TH.ListE <$> traverse (\(L _ e) -> convertExpr e) es
convertExpr (ExplicitTuple _ args boxity) = do
  args' <- traverse convertTupArg args
  Right
    ( case boxity of
        Boxed -> TH.TupE args'
        Unboxed -> TH.UnboxedTupE args'
    )
convertExpr (SectionL _ (L _ e) (L _ op)) = do
  e' <- convertExpr e
  op' <- convertExpr op
  Right (TH.InfixE (Just e') op' Nothing)
convertExpr (SectionR _ (L _ op) (L _ e)) = do
  op' <- convertExpr op
  e' <- convertExpr e
  Right (TH.InfixE Nothing op' (Just e'))
convertExpr (HsIf _ (L _ c) (L _ t) (L _ f)) = do
  c' <- convertExpr c
  t' <- convertExpr t
  f' <- convertExpr f
  Right (TH.CondE c' t' f')
convertExpr (HsLit _ lit) = TH.LitE <$> convertHsLit lit
convertExpr (HsOverLit _ ol) = TH.LitE <$> convertOverLit ol
convertExpr (ExprWithTySig _ (L _ e) sigWcTy) = do
  e' <- convertExpr e
  ty' <- convertSigWcType sigWcTy
  Right (TH.SigE e' ty')
convertExpr (HsGetField _ (L _ e) (L _ (DotFieldOcc _ (L _ fld)))) = do
  e' <- convertExpr e
  Right (TH.GetFieldE e' (fieldLabelToString fld))
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertExpr (HsProjection _ flds) =
  Right (TH.ProjectionE (fmap (\(DotFieldOcc _ (L _ fld)) -> fieldLabelToString fld) flds))
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
convertExpr (HsProjection _ flds) =
  Right (TH.ProjectionE (fmap (\(L _ (DotFieldOcc _ (L _ fld))) -> fieldLabelToString fld) flds))
#endif
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertExpr (HsAppType _ (L _ e) (HsWC _ (L _ ty))) = TH.AppTypeE <$> convertExpr e <*> convertType ty
#else
convertExpr (HsAppType _ (L _ e) _ (HsWC _ (L _ ty))) = TH.AppTypeE <$> convertExpr e <*> convertType ty
#endif
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertExpr (RecordCon _ (L _ conName) (HsRecFields _ flds _)) = do
  flds' <- traverse convertRecField flds
  Right $ TH.RecConE (rdrToName conName) flds'
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
convertExpr (RecordCon _ (L _ conName) (HsRecFields flds _)) = do
  flds' <- traverse convertRecField flds
  Right $ TH.RecConE (rdrToName conName) flds'
#endif
convertExpr (HsCase _ (L _ caseExpr) mg) = TH.CaseE <$> convertExpr caseExpr <*> convertMatchGroup mg

-- Now come our list of unsupported language features
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertExpr (HsEmbTy {}) = unsupportedLanguageFeatureMsg "Embedded type"
convertExpr (HsForAll {}) = unsupportedLanguageFeatureMsg "Forall-types"
convertExpr (HsFunArr {}) = unsupportedLanguageFeatureMsg "Function types"
convertExpr (HsQual {}) = unsupportedLanguageFeatureMsg "HsQual"
#else
convertExpr (HsLamCase {}) = unsupportedLanguageFeatureMsg "LambdaCase"
convertExpr (HsRecSel {}) = unsupportedLanguageFeatureMsg "Record field selectors"
#endif
convertExpr (HsUnboundVar {}) = unsupportedLanguageFeatureMsg "Unbound variables/holes"
convertExpr (HsOverLabel {}) = unsupportedLanguageFeatureMsg "Overloaded labels"
convertExpr (HsIPVar {}) = unsupportedLanguageFeatureMsg "Implicit parameters"
convertExpr (HsLam {}) = unsupportedLanguageFeatureMsg "Lambda"
convertExpr (ExplicitSum {}) = unsupportedLanguageFeatureMsg "Unboxed sums"
convertExpr (HsMultiIf {}) = unsupportedLanguageFeatureMsg "Multi-way if"
convertExpr (HsLet {}) = unsupportedLanguageFeatureMsg "Let"
convertExpr (HsDo {}) = unsupportedLanguageFeatureMsg "Do notation"
convertExpr (RecordUpd {}) = unsupportedLanguageFeatureMsg "Record updates"
convertExpr (ArithSeq {}) = unsupportedLanguageFeatureMsg "Arithmetic sequences"
convertExpr (HsTypedBracket {}) = unsupportedLanguageFeatureMsg "Typed Template Haskell brackets"
convertExpr (HsUntypedBracket {}) = unsupportedLanguageFeatureMsg "Untyped Template Haskell brackets"
convertExpr (HsTypedSplice {}) = unsupportedLanguageFeatureMsg "Typed Template Haskell splices"
convertExpr (HsUntypedSplice {}) = unsupportedLanguageFeatureMsg "Untyped Template Haskell splices"
convertExpr (HsProc {}) = unsupportedLanguageFeatureMsg "Arrow proc notation"
convertExpr (HsStatic {}) = unsupportedLanguageFeatureMsg "Static pointers"
convertExpr (HsPragE {}) = unsupportedLanguageFeatureMsg "Pragma"

unsupportedLanguageFeatureMsg :: String -> Either String a
unsupportedLanguageFeatureMsg feat = Left $ feat ++ " expressions are unsupported in hpgsql's SQL quasi-quoter. You can usually assign your expression to a binding outside the quasi-quoter and keep only that binding inside, but do raise an issue at https://github.com/mzabani/hpgsql/issues if you want this to be supported."

convertMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either String [TH.Match]
convertMatchGroup (MG _ (L _ matches)) = traverse convertMatch matches

convertMatch :: LMatch GhcPs (LHsExpr GhcPs) -> Either String TH.Match
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertMatch (L _ (Match _ _ (L _ pats) grhss)) = do
#else
convertMatch (L _ (Match _ _ pats grhss)) = do
#endif
  pats' <- traverse (\(L _ p) -> convertPat p) pats
  (body, decs) <- convertGRHSs grhss
  case pats' of
    [pat] -> Right (TH.Match pat body decs)
    _ -> unsupportedLanguageFeatureMsg "Multi-pattern matches"

convertGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> Either String (TH.Body, [TH.Dec])
convertGRHSs (GRHSs _ grhss localBinds) = do
  decs <- convertLocalBinds localBinds
  body <- case grhss of
    [L _ (GRHS _ [] (L _ e))] -> TH.NormalB <$> convertExpr e
    _ -> unsupportedLanguageFeatureMsg "Guarded case alternative"
  Right (body, decs)

convertLocalBinds :: HsLocalBinds GhcPs -> Either String [TH.Dec]
convertLocalBinds (EmptyLocalBinds _) = Right []
convertLocalBinds (HsValBinds {}) = unsupportedLanguageFeatureMsg "HsValBinds"
convertLocalBinds (HsIPBinds {}) = unsupportedLanguageFeatureMsg "HsIPBinds"

-- Pattern conversion (GHC Pat to TH Pat)

convertPat :: Pat GhcPs -> Either String TH.Pat
convertPat (WildPat _) = Right TH.WildP
convertPat (VarPat _ (L _ rdr)) = Right (TH.VarP (rdrToName rdr))
convertPat (LitPat _ lit) = TH.LitP <$> convertHsLit lit
convertPat (NPat _ (L _ ol) _ _) = TH.LitP <$> convertOverLit ol
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertPat (ConPat _ (L _ con) details) = convertConPatDetails con details
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
convertPat (ConPat _ (L _ con) details) = convertConPatDetails con details
#endif
convertPat (TuplePat _ pats boxity) = do
  pats' <- traverse (\(L _ p) -> convertPat p) pats
  Right $ case boxity of
    Boxed -> TH.TupP pats'
    Unboxed -> TH.UnboxedTupP pats'
convertPat (ListPat _ pats) = TH.ListP <$> traverse (\(L _ p) -> convertPat p) pats
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertPat (ParPat _ (L _ p)) = TH.ParensP <$> convertPat p
convertPat (AsPat _ (L _ rdr) (L _ p)) = TH.AsP (rdrToName rdr) <$> convertPat p
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
convertPat (ParPat _ _ (L _ p) _) = TH.ParensP <$> convertPat p
convertPat (AsPat _ (L _ rdr) _ (L _ p)) = TH.AsP (rdrToName rdr) <$> convertPat p
#endif
convertPat (BangPat _ (L _ p)) = TH.BangP <$> convertPat p
-- Unsupported pattern matching expressions
convertPat (LazyPat{}) = unsupportedLanguageFeatureMsg "LazyPat in pattern matching"
convertPat (ViewPat{}) = unsupportedLanguageFeatureMsg "ViewPat in pattern matching"
convertPat (SumPat{}) = unsupportedLanguageFeatureMsg "SumPat in pattern matching"
convertPat (SplicePat{}) = unsupportedLanguageFeatureMsg "SplicePat in pattern matching"
convertPat (SigPat{}) = unsupportedLanguageFeatureMsg "SigPat in pattern matching"
convertPat (NPlusKPat{}) = unsupportedLanguageFeatureMsg "NPlusKPat in pattern matching"
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertPat (EmbTyPat{}) = unsupportedLanguageFeatureMsg "EmbTyPat in pattern matching"
convertPat (InvisPat{}) = unsupportedLanguageFeatureMsg "InvisPat in pattern matching"
convertPat (OrPat{}) = unsupportedLanguageFeatureMsg "OrPat in pattern matching"
#endif

convertConPatDetails :: RdrName -> HsConPatDetails GhcPs -> Either String TH.Pat
convertConPatDetails con (PrefixCon tyArgs args) = do
  args' <- traverse (\(L _ p) -> convertPat p) args
  if null tyArgs
    then Right (TH.ConP (rdrToName con) [] args')
    else Left "Type applications in constructor patterns are unsupported in hpgsql's SQL quasi-quoter. Please file a bug report at https://github.com/mzabani/hpgsql/issues if you want this."
convertConPatDetails con (InfixCon (L _ l) (L _ r)) = do
  l' <- convertPat l
  r' <- convertPat r
  Right (TH.InfixP l' (rdrToName con) r')
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertConPatDetails con (RecCon (HsRecFields _ flds _)) = do
  flds' <- traverse convertPatRecField flds
  Right (TH.RecP (rdrToName con) flds')
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
convertConPatDetails con (RecCon (HsRecFields flds _)) = do
  flds' <- traverse convertPatRecField flds
  Right (TH.RecP (rdrToName con) flds')
#endif

convertPatRecField :: LHsRecField GhcPs (LPat GhcPs) -> Either String TH.FieldPat
convertPatRecField (L _ (HsFieldBind _ (L _ (FieldOcc _ (L _ rdr))) (L _ pat) _)) = do
  pat' <- convertPat pat
  Right (rdrToName rdr, pat')

-- Helper functions

rdrToExp :: RdrName -> TH.Exp
rdrToExp rdr =
  let name = rdrToName rdr
   in if isConstructorName name then TH.ConE name else TH.VarE name

rdrToName :: RdrName -> TH.Name
rdrToName (Unqual occ) = TH.mkName (occNameString occ)
rdrToName (Qual modN occ) = TH.mkName (moduleNameString modN ++ "." ++ occNameString occ)
rdrToName (Orig _ occ) = TH.mkName (occNameString occ)
rdrToName (Exact name) = TH.mkName (occNameString (nameOccName name))

isConstructorName :: TH.Name -> Bool
isConstructorName n = case TH.nameBase n of
  (c : _) -> isUpper c || c == ':'
  _ -> False

fieldLabelToString :: FieldLabelString -> String
fieldLabelToString (FieldLabelString fs) = unpackFS fs

convertRecField :: LHsRecField GhcPs (LHsExpr GhcPs) -> Either String (TH.Name, TH.Exp)
convertRecField (L _ (HsFieldBind _ (L _ (FieldOcc _ (L _ rdr))) (L _ expr) _)) = do
  expr' <- convertExpr expr
  Right (rdrToName rdr, expr')

convertTupArg :: HsTupArg GhcPs -> Either String (Maybe TH.Exp)
convertTupArg (Present _ (L _ e)) = Just <$> convertExpr e
convertTupArg (Missing _) = Right Nothing

convertHsLit :: HsLit GhcPs -> Either String TH.Lit
convertHsLit (HsChar _ c) = Right (TH.CharL c)
convertHsLit (HsString _ fs) = Right (TH.StringL (unpackFS fs))
convertHsLit (HsInt _ il) = Right (TH.IntegerL (il_value il))
convertHsLit (HsIntPrim _ i) = Right (TH.IntPrimL i)
convertHsLit (HsWordPrim _ w) = Right (TH.WordPrimL w)
convertHsLit (HsFloatPrim _ fl) = Right (TH.FloatPrimL (rationalFromFractionalLit fl)) -- TODO Why rational?
convertHsLit (HsDoublePrim _ fl) = Right (TH.DoublePrimL (rationalFromFractionalLit fl))
#if MIN_VERSION_ghc_lib_parser(9,10,0)
convertHsLit (HsMultilineString _ fs) = Right (TH.StringL (unpackFS fs))
#endif
convertHsLit (HsCharPrim {}) = unsupportedLanguageFeatureMsg "HsCharPrim literal"
convertHsLit (HsStringPrim {}) = unsupportedLanguageFeatureMsg "HsStringPrim literal"
convertHsLit (HsInt8Prim {}) = unsupportedLanguageFeatureMsg "HsInt8Prim literal"
convertHsLit (HsInt16Prim {}) = unsupportedLanguageFeatureMsg "HsInt16Prim literal"
convertHsLit (HsInt32Prim {}) = unsupportedLanguageFeatureMsg "HsInt32Prim literal"
convertHsLit (HsInt64Prim {}) = unsupportedLanguageFeatureMsg "HsInt64Prim literal"
convertHsLit (HsWord8Prim {}) = unsupportedLanguageFeatureMsg "HsWord8Prim literal"
convertHsLit (HsWord16Prim {}) = unsupportedLanguageFeatureMsg "HsWord16Prim literal"
convertHsLit (HsWord32Prim {}) = unsupportedLanguageFeatureMsg "HsWord32Prim literal"
convertHsLit (HsWord64Prim {}) = unsupportedLanguageFeatureMsg "HsWord64Prim literal"
convertHsLit (HsInteger {}) = unsupportedLanguageFeatureMsg "HsInteger literal"
convertHsLit (HsRat {}) = unsupportedLanguageFeatureMsg "HsRat literal"

convertOverLit :: HsOverLit GhcPs -> Either String TH.Lit
convertOverLit ol = case ol_val ol of
  HsIntegral il -> Right (TH.IntegerL (il_value il))
  HsFractional fl -> Right (TH.RationalL (rationalFromFractionalLit fl))
  HsIsString _ fs -> Right (TH.StringL (unpackFS fs))

-- Type conversion (GHC HsType to TH Type)

convertSigWcType :: LHsSigWcType GhcPs -> Either String TH.Type
convertSigWcType (HsWC _ (L _ (HsSig _ _ (L _ ty)))) = convertType ty

convertType :: HsType GhcPs -> Either String TH.Type
convertType (HsTyVar _ promo (L _ rdr)) =
  let name = rdrToName rdr
   in Right $ case promo of
        IsPromoted -> TH.PromotedT name
        NotPromoted
          | isConstructorName name -> TH.ConT name
          | otherwise -> TH.VarT name
convertType (HsAppTy _ (L _ t1) (L _ t2)) =
  TH.AppT <$> convertType t1 <*> convertType t2
convertType (HsListTy _ (L _ t)) =
  TH.AppT TH.ListT <$> convertType t
convertType (HsTupleTy _ _ ts) = do
  ts' <- traverse (\(L _ t) -> convertType t) ts
  let n = length ts'
  Right (foldl TH.AppT (TH.TupleT n) ts')
convertType (HsFunTy _ _ (L _ t1) (L _ t2)) =
  TH.AppT . TH.AppT TH.ArrowT <$> convertType t1 <*> convertType t2
convertType (HsParTy _ (L _ t)) =
  convertType t
convertType (HsQualTy _ _ (L _ t)) =
  convertType t
convertType (HsForAllTy{}) = unsupportedLanguageFeatureMsg "HsForAllTy in a type"
convertType (HsAppKindTy{}) = unsupportedLanguageFeatureMsg "HsAppKindTy in a type"
convertType (HsOpTy{}) = unsupportedLanguageFeatureMsg "HsOpTy in a type"
convertType (HsSumTy{}) = unsupportedLanguageFeatureMsg "HsSumTy in a type"
convertType (HsIParamTy{}) = unsupportedLanguageFeatureMsg "HsIParamTy in a type"
convertType (HsStarTy{}) = unsupportedLanguageFeatureMsg "HsStarTy in a type"
convertType (HsKindSig{}) = unsupportedLanguageFeatureMsg "HsKindSig in a type"
convertType (HsSpliceTy{}) = unsupportedLanguageFeatureMsg "HsSpliceTy in a type"
convertType (HsDocTy{}) = unsupportedLanguageFeatureMsg "HsDocTy in a type"
convertType (HsBangTy{}) = unsupportedLanguageFeatureMsg "HsBangTy in a type"
convertType (HsRecTy{}) = unsupportedLanguageFeatureMsg "HsRecTy in a type"
convertType (HsExplicitListTy{}) = unsupportedLanguageFeatureMsg "HsExplicitListTy in a type"
convertType (HsExplicitTupleTy{}) = unsupportedLanguageFeatureMsg "HsExplicitTupleTy in a type"
convertType (HsTyLit{}) = unsupportedLanguageFeatureMsg "HsTyLit in a type"
convertType (HsWildCardTy{}) = unsupportedLanguageFeatureMsg "HsWildCardTy in a type"
convertType (XHsType{}) = unsupportedLanguageFeatureMsg "XHsType in a type"
