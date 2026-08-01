{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Hpgsql.GhcParseExp (parseExp, canParseExp) where

import Data.Char (isUpper)
import Data.Either (isRight)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import GHC.Hs hiding (UnicodeSyntax)
import GHC.Parser (parseExpression)
import GHC.Parser.Lexer (P (..), ParseResult (..), initParserState)
import GHC.Parser.PostProcess (ECP (..), runPV)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SourceText (IntegralLit (..), rationalFromFractionalLit)
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc)
import Hpgsql.GhcParserOpts (fakeSettings)
import Hpgsql.LanguageHaskell.FromThExtension (fromThToGhcLibExtension)
import Language.Haskell.Syntax.Basic (FieldLabelString (..))
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
convertExpr (HsOverLit _ ol) = convertOverLit ol
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
convertExpr (HsAppType _ (L _ e) (HsWC _ (L _ ty))) = TH.AppTypeE <$> convertExpr e <*> convertType ty
-- convertExpr (HsAppType _ _ _) = Left "TypeApplications are still unsupported in hpgsql's SQL quasi-quoter. Please file a bug report at https://github.com/mzabani/hpgsql/issues if you want this."
convertExpr _ = Left "Unsupported Haskell expression form in hpgsql's SQL quasi-quoter"

-- Helper functions

rdrToExp :: RdrName -> TH.Exp
rdrToExp rdr =
  let name = rdrToName rdr
   in if isConName name then TH.ConE name else TH.VarE name

rdrToName :: RdrName -> TH.Name
rdrToName (Unqual occ) = TH.mkName (occNameString occ)
rdrToName (Qual modN occ) = TH.mkName (moduleNameString modN ++ "." ++ occNameString occ)
rdrToName _ = TH.mkName "<unknown-name>"

isConName :: TH.Name -> Bool
isConName n = case TH.nameBase n of
  -- TODO: No module name check?
  (c : _) -> isUpper c || c == ':'
  _ -> False

fieldLabelToString :: FieldLabelString -> String
fieldLabelToString (FieldLabelString fs) = unpackFS fs

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
convertHsLit _ = Left "Unsupported literal type in SQL quasi-quoter"

convertOverLit :: HsOverLit GhcPs -> Either String TH.Exp
convertOverLit ol = case ol_val ol of
  HsIntegral il -> Right (TH.LitE (TH.IntegerL (il_value il)))
  HsFractional fl -> Right (TH.LitE (TH.RationalL (rationalFromFractionalLit fl)))
  HsIsString _ fs -> Right (TH.LitE (TH.StringL (unpackFS fs)))

-- Type conversion (GHC HsType to TH Type)

convertSigWcType :: LHsSigWcType GhcPs -> Either String TH.Type
convertSigWcType (HsWC _ (L _ (HsSig _ _ (L _ ty)))) = convertType ty

convertType :: HsType GhcPs -> Either String TH.Type
convertType (HsTyVar _ promo (L _ rdr)) =
  let name = rdrToName rdr
   in Right $ case promo of
        IsPromoted -> TH.PromotedT name
        NotPromoted
          | isConName name -> TH.ConT name
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
convertType _ = Left "Unsupported type in SQL quasi-quoter type signature"
