{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hpgsql.Query
  ( Query (..), -- We probably shouldn't export this ctor?
    SingleQuery (..), -- Nor this one
    sql,
    commaSeparatedRowTuples,
    mkQueryInternal,
    breakQueryIntoStatements,
    mkQuery,
    escapeIdentifier,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (rights)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hpgsql.Base (lastTwoAndInit, maximumOnOrDef, minimumOnOrDef)
import Hpgsql.Builder (BinaryField)
import Hpgsql.Encoding (ToPgField (..), ToPgRow (..))
import Hpgsql.Parsing (BlockOrNotBlock (..), ParsingOpts (..), QQExprKind (..), blockText, parseSql)
import Hpgsql.TypeInfo (EncodingContext, Oid)
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

escapeIdentifier :: ByteString -> Query
escapeIdentifier v = Query {queryString = [FragmentOfStaticSql "\"", FragmentOfStaticSql (doubleQuotes v), FragmentOfStaticSql "\""], queryParams = []}
  where
    doubleQuotes = BS.intercalate "\"\"" . BS.split 0x22 {- '"' -}

-- | A useful representation for our quasiquoter parsing.
data SqlFragment
  = NonInterpolatedSqlFragment !Text
  | InterpolatedHaskellExpr !Text
  | EmbeddedQueryExpr !Text
  | SemiColonFragment
  | WhitespaceOrCommentsFragment !Text
  deriving stock (Eq)

-- | A fragment of a single SQL statement, which is either static SQL or
-- a placeholder for a query argument.
data SingleQueryFragment
  = FragmentOfStaticSql !ByteString
  | FragmentWithSemiColon
  | FragmentOfCommentsOrWhitespace !ByteString
  | -- | The number/index of the query argument, starting from 1 in a single statement
    QueryArgumentPlaceHolder !Int
  deriving stock (Eq, Show)

-- | Zero, one, or more SQL statements. The query parameters and query fragments continue to go up in number across different
-- statements, e.g. "SELECT $1; SELECT $2; SELECT $3; ...".
data Query = Query {queryString :: ![SingleQueryFragment], queryParams :: ![EncodingContext -> (Maybe Oid, BinaryField)]}

instance Show Query where
  show = show . NE.toList . breakQueryIntoStatements

-- | A single statement, not multiple, with dollar-numbered query arguments
-- starting from $1.
data SingleQuery = SingleQuery {queryString :: !ByteString, queryParams :: ![EncodingContext -> (Maybe Oid, BinaryField)]}

instance Show SingleQuery where
  -- Careful not exposing query arguments
  show (SingleQuery {queryString}) = show queryString

-- | Takes in a query string with query arguments as dollar-numbered arguments and returns a Query you
-- can run.
-- Examples:
-- - "SELECT * FROM table WHERE col1=$1 AND col2=$2"
-- - "SELECT * FROM table WHERE (["a", "b", "c"]'::jsonb ? 'b') = $1"
-- Question marks are interpreted literally, i.e. they have no special meaning.
-- Note that if the number of arguments does not match what's in the query string,
-- this will throw an error.
mkQuery :: (ToPgRow a) => ByteString -> a -> Query
mkQuery qryText p = mkQueryInternalFromSqlStatements (parseSql AcceptOnlyDollarNumberedArgs $ decodeUtf8 qryText) (toPgParams p)
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
       in Query {queryString = queryFrags, queryParams = allParams}

-- | Meant for internal usage, helps build "VALUES (..), (..)"-like statements.
-- Users of hpgsql should just use the `Values` type instead of this.
commaSeparatedRowTuples :: [[EncodingContext -> (Maybe Oid, BinaryField)]] -> Query
commaSeparatedRowTuples rowTuples =
  let (_, queryFrags) =
        List.mapAccumR
          ( \(!maxArgSoFar) singleRow ->
              let numParams = length singleRow
                  numberedArgs = map (QueryArgumentPlaceHolder . (+ maxArgSoFar)) [1 .. numParams]
               in (maxArgSoFar + numParams, FragmentOfStaticSql "(" : (List.intersperse (FragmentOfStaticSql ",") numberedArgs ++ [FragmentOfStaticSql "),"]))
          )
          0
          rowTuples
   in Query {queryString = mconcat queryFrags, queryParams = mconcat rowTuples}

instance Semigroup Query where
  q1 <> q2 =
    let maxArgQ1 =
          maximumOnOrDef
            0
            q1.queryString
            ( \case
                QueryArgumentPlaceHolder n -> Just n
                _ -> Nothing
            )
        (_, remappedQ2) =
          renumberParamsFrom
            q2.queryString
            (maxArgQ1 + 1)
     in Query {queryString = q1.queryString <> remappedQ2, queryParams = q1.queryParams <> q2.queryParams}

-- | For internal usage only. Takes a `Query` and breaks it up into
-- individual SQL statements that can be sent to a postgres backend.
breakQueryIntoStatements :: Query -> NonEmpty SingleQuery
breakQueryIntoStatements qry@Query {queryString = fullQueryString, queryParams = allQueryParams} =
  -- If a user insists in trying to run an empty query, or if
  -- they mistakenly concatenate empty statements, we let them
  toEmptyQueryIfNecessary $
    map toSingleQuery $
      -- If a query string is "SELECT 1; -- comments and empty space", we put
      -- all comments and whitespace together with that last "real" SQL statement
      fixLastEmptyStatement $
        go fullQueryString allQueryParams
  where
    toEmptyQueryIfNecessary [] = NE.singleton $ SingleQuery {queryString = "", queryParams = allQueryParams}
    toEmptyQueryIfNecessary (x : xs) = x :| xs
    toSingleQuery (blks, prms) = SingleQuery {queryString = mconcat $ map fragToBytestring blks, queryParams = prms}
    allWhitespaceOrComments =
      all
        ( \case
            FragmentOfCommentsOrWhitespace _ -> True
            _ -> False
        )
    fixLastEmptyStatement :: [([SingleQueryFragment], [EncodingContext -> (Maybe Oid, BinaryField)])] -> [([SingleQueryFragment], [EncodingContext -> (Maybe Oid, BinaryField)])]
    fixLastEmptyStatement indivStmts = case lastTwoAndInit indivStmts of
      (_, Nothing) -> indivStmts -- Only 0 or 1 statements found
      (firstStmts, Just (secLst, lst))
        | allWhitespaceOrComments (fst lst) -> firstStmts ++ [secLst <> lst]
        | otherwise -> indivStmts
    isLastFragmentOfAStatement = \case
      FragmentWithSemiColon -> True
      _ -> False
    fragToBytestring = \case
      QueryArgumentPlaceHolder n -> "$" <> intToBs n
      FragmentOfStaticSql t -> t
      FragmentWithSemiColon -> ";"
      FragmentOfCommentsOrWhitespace t -> t
    go :: [SingleQueryFragment] -> [EncodingContext -> (Maybe Oid, BinaryField)] -> [([SingleQueryFragment], [EncodingContext -> (Maybe Oid, BinaryField)])]
    go [] [] = []
    go [] _ = error $ "Hpgsql error: empty query fragment list but outstanding query params. Number of query arguments is " ++ show (length allQueryParams) ++ " and query is " ++ show qry
    go frags params =
      let (stmtFrags, nextFrags) = case List.break isLastFragmentOfAStatement frags of
            (firstStmts, []) -> (firstStmts, [])
            (firstStmts, semiColon : next) -> (firstStmts ++ [semiColon], next)
          (maxArgNum, thisQueryFrags) = renumberParamsFrom stmtFrags 1
          (thisQueryParams, nextParams) = List.splitAt maxArgNum params
       in (thisQueryFrags, thisQueryParams) : go nextFrags nextParams

intToBs :: Int -> ByteString
intToBs = encodeUtf8 . Text.pack . show

instance IsString Query where
  fromString s =
    -- For a string to be a `Query` all by itself, it means it can't have query
    -- arguments. Users should use the quasiquoter or `mkQuery` for query arguments.
    mkQuery (encodeUtf8 $ Text.pack s) ()

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
   in Query {queryString = mconcat queryFrags, queryParams = concatMap rights allParams}

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = liftQuery . parseSql AcceptQuasiQuoterExpressions . Text.pack,
      quotePat = error "Hpgsql's sql quasiquoter does not implement quotePat",
      quoteType = error "Hpgsql's sql quasiquoter does not implement quoteType",
      quoteDec = error "Hpgsql's sql quasiquoter does not implement quoteDec"
    }

liftQuery :: [BlockOrNotBlock] -> Q Exp
liftQuery stmt = do
  let allFragments = concatMap parseBlockQuasiQuoter stmt
      hasEmbedded = any isEmbedded allFragments
  if hasEmbedded
    then liftQueryDynamic allFragments
    else liftQueryStatic allFragments
  where
    isEmbedded (EmbeddedQueryExpr _) = True
    isEmbedded _ = False

-- | Static path: no ^{} expressions, SQL string is a compile-time literal.
liftQueryStatic :: [SqlFragment] -> Q Exp
liftQueryStatic allFragments = do
  let (fragQExps, varNames) = buildQueryFragsAndVars allFragments 1
  fragExps <- sequence fragQExps
  paramExps <- mapM generateParamExp varNames
  [|Query $(pure $ ListE fragExps) $(pure $ ListE paramExps)|]

-- | Dynamic path: has ^{} expressions, build query at runtime via buildQueryQQ.
liftQueryDynamic :: [SqlFragment] -> Q Exp
liftQueryDynamic allFragments = do
  partExps <- mapM fragmentToPartExp allFragments
  [|buildQueryQQ $(pure $ ListE partExps)|]

-- | Convert a SqlFragment into a TH expression producing a QueryBuildPart.
fragmentToPartExp :: SqlFragment -> Q Exp
fragmentToPartExp (NonInterpolatedSqlFragment t) =
  [|StaticSqlPart $(litE (stringL (Text.unpack t)))|]
fragmentToPartExp (InterpolatedHaskellExpr haskellExpr) =
  case parseExp (Text.unpack haskellExpr) of
    Left err -> error $ "Could not parse Haskell expression '" ++ Text.unpack haskellExpr ++ "': " ++ err
    Right expr -> [|ParamPart (\encCtx -> (toTypeOid (proxyOf $(pure expr)) encCtx, toPgField encCtx $(pure expr)))|]
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

buildQueryQQ :: [QueryBuildPartQQ] -> Query
buildQueryQQ parts =
  let (queryFrags, allParams) = go parts 1
   in Query {queryString = queryFrags, queryParams = allParams}
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

-- | Returns new fragments remapped with `renumberFrom` as the smallest query argument number,
-- and also returns the maximum (new) query argument number, or 0 if there were no query arguments.
renumberParamsFrom :: [SingleQueryFragment] -> Int -> (Int, [SingleQueryFragment])
renumberParamsFrom frags renumberFrom =
  List.mapAccumR
    ( \(!maxSoFar) -> \case
        QueryArgumentPlaceHolder n -> let newNum = n - smallestArgNum + renumberFrom in (max newNum maxSoFar, QueryArgumentPlaceHolder newNum)
        x -> (maxSoFar, x)
    )
    0
    frags
  where
    smallestArgNum =
      minimumOnOrDef
        1
        frags
        ( \case
            QueryArgumentPlaceHolder n -> Just n
            _ -> Nothing
        )

parseBlockQuasiQuoter :: BlockOrNotBlock -> [SqlFragment]
parseBlockQuasiQuoter (StaticSql text) = [NonInterpolatedSqlFragment text]
parseBlockQuasiQuoter SemiColon = [SemiColonFragment]
parseBlockQuasiQuoter (CommentsOrWhitespace text) = [WhitespaceOrCommentsFragment text]
parseBlockQuasiQuoter (DollarNumberedArg n) = [NonInterpolatedSqlFragment $ "$" <> Text.pack (show n)] -- If someone uses e.g. $2 in a quasiquoter, it should be an error, but let's assume they know what they're doing and treat it as text
parseBlockQuasiQuoter QuestionMarkArg = [NonInterpolatedSqlFragment "?"] -- If someone uses a question mark in a quasiquoter, it should be an error, but let's assume they know what they're doing and treat it as text
parseBlockQuasiQuoter (QuasiQuoterExpression QQInterpolation expr) = [InterpolatedHaskellExpr expr]
parseBlockQuasiQuoter (QuasiQuoterExpression QQEmbeddedQuery expr) = [EmbeddedQueryExpr expr]

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | Generate a parameter expression for a captured variable
generateParamExp :: Text -> Q Exp
generateParamExp (Text.unpack -> haskellExpr) =
  case parseExp haskellExpr of
    Left err -> error $ "Could not parse Haskell expression '" ++ haskellExpr ++ "': " ++ err
    Right expr ->
      [|\encCtx -> (toTypeOid (proxyOf $(pure expr)) encCtx, toPgField encCtx $(pure expr))|]
