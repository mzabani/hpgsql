module Hpgsql.QueryInternal
  ( Query (..),
    SingleQuery (..),
    sql,
    sqlPrep,
    mkQueryInternal,
    breakQueryIntoStatements,
    mkQuery,
    encodeParam,
  )
where

import Data.ByteString (ByteString)
import Data.Either (rights)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hpgsql.Builder (BinaryField)
import Hpgsql.Encoding (FieldEncoder (..), RowEncoder (..), ToPgField (..), ToPgRow (..))
import Hpgsql.InternalTypes (Query (..), SingleQuery (..), SingleQueryFragment (..), breakQueryIntoStatements, renumberParamsFrom)
import Hpgsql.Parsing (BlockOrNotBlock (..), ParsingOpts (..), QQExprKind (..), blockText, parseSql)
import Hpgsql.TypeInfo (EncodingContext, Oid)
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | A useful representation for our quasiquoter parsing.
data SqlFragment
  = NonInterpolatedSqlFragment !Text
  | InterpolatedHaskellExpr !Text
  | EmbeddedQueryExpr !Text
  | SemiColonFragment
  | WhitespaceOrCommentsFragment !Text
  deriving stock (Eq)

-- | Takes in a query string with query arguments as dollar-numbered arguments and returns a Query you
-- can run.
-- Examples:
-- - "SELECT * FROM table WHERE col1=$1 AND col2=$2"
-- - "SELECT * FROM table WHERE (["a", "b", "c"]'::jsonb ? 'b') = $1"
-- Question marks are interpreted literally, i.e. they have no special meaning.
-- Note that if the number of arguments does not match what's in the query string,
-- this will throw an error.
mkQuery :: (ToPgRow a) => ByteString -> a -> Query
mkQuery qryText p = mkQueryInternalFromSqlStatements (parseSql AcceptOnlyDollarNumberedArgs $ decodeUtf8 qryText) (toRowEncoder.toPgParams p)
  where
    mkQueryInternalFromSqlStatements :: [BlockOrNotBlock] -> [EncodingContext -> (Maybe Oid, BinaryField)] -> Query
    mkQueryInternalFromSqlStatements blocks allParams =
      let paramsLen = length allParams
          qryTextForError = show $ mconcat $ map blockText blocks
          queryFrags =
            map
              ( \case
                  StaticSql t -> FragmentOfStaticSql $ encodeUtf8 t
                  SemiColon -> FragmentWithSemiColon
                  CommentsOrWhitespace t -> FragmentOfCommentsOrWhitespace $ encodeUtf8 t
                  DollarNumberedArg n ->
                    if n < 1
                      then
                        error $ "Dollar-numbered query argument placeholders must start from 1. Query: " ++ qryTextForError
                      else
                        if n > paramsLen
                          then
                            error $ "Query contains more dollar-numbered query arguments than actually supplied query arguments. Query: " ++ qryTextForError
                          else QueryArgumentPlaceHolder n
                  QuestionMarkArg ->
                    error $ "Bug in Hpgsql: parseSql AcceptOnlyDollarNumberedArgs returned question mark place holders. Query: " ++ qryTextForError
                  QuasiQuoterExpression _ _ ->
                    error $ "Bug in Hpgsql: parseSql AcceptOnlyDollarNumberedArgs returned quasiquoter expression. Query: " ++ qryTextForError
              )
              blocks
       in Query {queryString = queryFrags, queryParams = allParams, isPrepared = False}

-- | Takes in a query string with query arguments as question marks (NOT dollar-numbered arguments).
-- Example:
-- - "SELECT * FROM table WHERE col1=? AND col2=?"
-- You should really use `mkQuery` instead of this. This is here only for hpgsql-simple-compat.
mkQueryInternal :: ByteString -> [[Either ByteString (EncodingContext -> (Maybe Oid, BinaryField))]] -> Query
mkQueryInternal queryTemplate allParams =
  let statements = parseSql AcceptQuestionMarksAsQueryArgs (decodeUtf8 queryTemplate)
      paramsByIdx = Map.fromList $ zip [(1 :: Int) ..] allParams
      qryTextForError = mconcat $ map blockText statements
      (_, queryFrags) =
        List.mapAccumL
          ( \(!maxArgSoFar, !maxRealArgSoFar) sqlPiece -> case sqlPiece of
              StaticSql t -> ((maxArgSoFar, maxRealArgSoFar), [FragmentOfStaticSql $ encodeUtf8 t])
              SemiColon -> ((maxArgSoFar, maxRealArgSoFar), [FragmentWithSemiColon])
              CommentsOrWhitespace t -> ((maxArgSoFar, maxRealArgSoFar), [FragmentOfCommentsOrWhitespace $ encodeUtf8 t])
              DollarNumberedArg _ ->
                error $ "Bug in Hpgsql: parsed a DollarNumberedArg in mkQueryInternal. Query: " ++ show (queryTemplate, qryTextForError)
              QuasiQuoterExpression _ _ ->
                error $ "Bug in Hpgsql: parsed a QuasiQuoterExpression in mkQueryInternal. Query: " ++ show (queryTemplate, qryTextForError)
              QuestionMarkArg ->
                let thisParamNum = maxArgSoFar + 1
                 in case Map.lookup thisParamNum paramsByIdx of
                      Nothing -> error $ "Could not find query argument of number " <> show thisParamNum <> " for query with " <> show (length allParams) <> " arguments supplied. Did you supply an insufficient amount of query arguments? Query is " ++ show qryTextForError
                      Just args ->
                        let (newMaxRealArg, frags) =
                              List.mapAccumL
                                ( \(!newArgNum) arg -> case arg of
                                    Left fakeArg ->
                                      (newArgNum, FragmentOfStaticSql fakeArg)
                                    Right _properArg ->
                                      (newArgNum + 1, QueryArgumentPlaceHolder $ newArgNum + 1)
                                )
                                maxRealArgSoFar
                                args
                         in ((thisParamNum, newMaxRealArg), frags)
          )
          (0, 0)
          statements
   in Query {queryString = mconcat queryFrags, queryParams = concatMap rights allParams, isPrepared = False}

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = liftQuery False . parseSql AcceptQuasiQuoterExpressions . Text.pack,
      quotePat = error "Hpgsql's sql quasiquoter does not implement quotePat",
      quoteType = error "Hpgsql's sql quasiquoter does not implement quoteType",
      quoteDec = error "Hpgsql's sql quasiquoter does not implement quoteDec"
    }

sqlPrep :: QuasiQuoter
sqlPrep =
  QuasiQuoter
    { quoteExp = liftQuery True . parseSql AcceptQuasiQuoterExpressions . Text.pack,
      quotePat = error "Hpgsql's sql quasiquoter does not implement quotePat",
      quoteType = error "Hpgsql's sql quasiquoter does not implement quoteType",
      quoteDec = error "Hpgsql's sql quasiquoter does not implement quoteDec"
    }

liftQuery :: Bool -> [BlockOrNotBlock] -> Q Exp
liftQuery isPrepared stmt = do
  let allFragments = concatMap parseBlockQuasiQuoter stmt
      hasEmbedded = any isEmbedded allFragments
  if hasEmbedded
    then liftQueryDynamic isPrepared allFragments
    else liftQueryStatic isPrepared allFragments
  where
    isEmbedded (EmbeddedQueryExpr _) = True
    isEmbedded _ = False

-- | Static path: no ^{} expressions, SQL string is a compile-time literal.
liftQueryStatic :: Bool -> [SqlFragment] -> Q Exp
liftQueryStatic isPrepared allFragments = do
  let (fragQExps, varNames) = buildQueryFragsAndVars allFragments 1
  fragExps <- sequence fragQExps
  paramExps <- mapM generateParamExp varNames
  [|Query $(pure $ ListE fragExps) $(pure $ ListE paramExps) isPrepared|]

-- | Dynamic path: has ^{} expressions, build query at runtime via buildQueryQQ.
liftQueryDynamic :: Bool -> [SqlFragment] -> Q Exp
liftQueryDynamic isPrepared allFragments = do
  partExps <- mapM fragmentToPartExp allFragments
  [|buildQueryQQ isPrepared $(pure $ ListE partExps)|]

-- | Convert a SqlFragment into a TH expression producing a QueryBuildPart.
fragmentToPartExp :: SqlFragment -> Q Exp
fragmentToPartExp (NonInterpolatedSqlFragment t) =
  [|StaticSqlPart $(litE (stringL (Text.unpack t)))|]
fragmentToPartExp (InterpolatedHaskellExpr haskellExpr) =
  case parseExp (Text.unpack haskellExpr) of
    Left err -> error $ "Could not parse Haskell expression '" ++ Text.unpack haskellExpr ++ "': " ++ err
    Right expr -> [|ParamPart (encodeParam $(pure expr))|]
fragmentToPartExp SemiColonFragment =
  [|SemiColonPart|]
fragmentToPartExp (WhitespaceOrCommentsFragment t) =
  [|WhitespaceOrCommenstPart $(litE (stringL (Text.unpack t)))|]
fragmentToPartExp (EmbeddedQueryExpr haskellExpr) =
  case parseExp (Text.unpack haskellExpr) of
    Left err -> error $ "Could not parse Haskell expression '" ++ Text.unpack haskellExpr ++ "': " ++ err
    Right expr -> [|EmbeddedQueryPart $(pure expr)|]

-- | Walk through fragments in order, building SingleQueryFragment TH expressions
-- and collecting the interpolated Haskell expressions.
buildQueryFragsAndVars :: [SqlFragment] -> Int -> ([Q Exp], [Text])
buildQueryFragsAndVars [] _ = ([], [])
buildQueryFragsAndVars (NonInterpolatedSqlFragment t : rest) n =
  let (restFrags, restVars) = buildQueryFragsAndVars rest n
   in ([|FragmentOfStaticSql (encodeUtf8 $(litE (stringL (Text.unpack t))))|] : restFrags, restVars)
buildQueryFragsAndVars (WhitespaceOrCommentsFragment t : rest) n =
  let (restFrags, restVars) = buildQueryFragsAndVars rest n
   in ([|FragmentOfCommentsOrWhitespace (encodeUtf8 $(litE (stringL (Text.unpack t))))|] : restFrags, restVars)
buildQueryFragsAndVars (InterpolatedHaskellExpr var : rest) n =
  let (restFrags, restVars) = buildQueryFragsAndVars rest (n + 1)
   in ([|QueryArgumentPlaceHolder $(litE (integerL (fromIntegral n)))|] : restFrags, var : restVars)
buildQueryFragsAndVars (SemiColonFragment : rest) n =
  let (restFrags, restVars) = buildQueryFragsAndVars rest n
   in ([|FragmentWithSemiColon|] : restFrags, restVars)
buildQueryFragsAndVars (EmbeddedQueryExpr _ : _) _ =
  error "Bug in Hpgsql: EmbeddedQueryExpr should not appear in static path"

-- | Parts used to build a SingleQuery at runtime when ^{} embedded queries are present.
data QueryBuildPartQQ
  = StaticSqlPart !ByteString
  | ParamPart !(EncodingContext -> (Maybe Oid, BinaryField))
  | EmbeddedQueryPart !Query
  | SemiColonPart
  | WhitespaceOrCommenstPart !ByteString

buildQueryQQ :: Bool -> [QueryBuildPartQQ] -> Query
buildQueryQQ isPrepared parts =
  let (queryFrags, allParams) = go parts 1
   in Query {queryString = queryFrags, queryParams = allParams, isPrepared}
  where
    go [] _ = ([], [])
    go (part : rest) argNum = case part of
      StaticSqlPart bs ->
        let (restFrags, restParams) = go rest argNum
         in (FragmentOfStaticSql bs : restFrags, restParams)
      WhitespaceOrCommenstPart bs ->
        let (restFrags, restParams) = go rest argNum
         in (FragmentOfCommentsOrWhitespace bs : restFrags, restParams)
      ParamPart p ->
        let (restFrags, restParams) = go rest (argNum + 1)
         in (QueryArgumentPlaceHolder argNum : restFrags, p : restParams)
      SemiColonPart ->
        let (restFrags, restParams) = go rest argNum
         in (FragmentWithSemiColon : restFrags, restParams)
      EmbeddedQueryPart q ->
        let (newMaxArgNum, renumberedFrags) = renumberParamsFrom q.queryString argNum
            (restFrags, restParams) = go rest (newMaxArgNum + 1)
         in (renumberedFrags ++ restFrags, q.queryParams ++ restParams)

parseBlockQuasiQuoter :: BlockOrNotBlock -> [SqlFragment]
parseBlockQuasiQuoter (StaticSql text) = [NonInterpolatedSqlFragment text]
parseBlockQuasiQuoter SemiColon = [SemiColonFragment]
parseBlockQuasiQuoter (CommentsOrWhitespace text) = [WhitespaceOrCommentsFragment text]
parseBlockQuasiQuoter (DollarNumberedArg n) = [NonInterpolatedSqlFragment $ "$" <> Text.pack (show n)] -- If someone uses e.g. $2 in a quasiquoter, it should be an error, but let's assume they know what they're doing and treat it as text
parseBlockQuasiQuoter QuestionMarkArg = [NonInterpolatedSqlFragment "?"] -- If someone uses a question mark in a quasiquoter, it should be an error, but let's assume they know what they're doing and treat it as text
parseBlockQuasiQuoter (QuasiQuoterExpression QQInterpolation expr) = [InterpolatedHaskellExpr expr]
parseBlockQuasiQuoter (QuasiQuoterExpression QQEmbeddedQuery expr) = [EmbeddedQueryExpr expr]

-- | Generate a parameter expression for a captured variable
generateParamExp :: Text -> Q Exp
generateParamExp (Text.unpack -> haskellExpr) =
  case parseExp haskellExpr of
    Left err -> error $ "Could not parse Haskell expression '" ++ haskellExpr ++ "': " ++ err
    Right expr ->
      [|encodeParam $(pure expr)|]

encodeParam :: (ToPgField a) => a -> EncodingContext -> (Maybe Oid, BinaryField)
encodeParam v =
  let fe = fieldEncoder
   in \encCtx -> (fe.toTypeOid encCtx, fe.toPgField encCtx v)
