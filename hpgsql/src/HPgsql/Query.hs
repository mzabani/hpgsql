{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPgsql.Query
  ( Query (..), -- We probably shouldn't export this ctor?
    SingleQuery (..), -- Nor this one
    sql,
    commaSeparatedRowTuples,
    mkQueryInternal,
    breakQueryIntoStatements,
    mkQuery,
  )
where

import Control.Applicative (some, (<|>))
import Control.Monad (foldM)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import HPgsql.Base (maximumOnOrDef, minimumOnOrDef)
import HPgsql.Encoding (ToPgField (..), ToPgRow (..))
import HPgsql.Parsing (BlockOrNotBlock (..), SqlStatement (..), parseSql, sqlStatementText)
import HPgsql.TypeInfo (Oid, TypeInfo)
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | A useful representation for our quasiquoter parsing.
data SqlFragment
  = NonInterpolatedSqlFragment !Text
  | InterpolatedHaskellExpr !Text
  | -- | @^{expr}@ where @expr :: Query@
    EmbeddedQueryExpr !Text
  deriving stock (Eq)

-- | A fragment of a single SQL statement, which is either static SQL or
-- a placeholder for a query argument.
data SingleQueryFragment
  = FragmentOfStaticSql !ByteString
  | -- | The number/index of the query argument, starting from 1 in a single statement
    QueryArgumentPlaceHolder !Int
  deriving stock (Eq, Show)

-- | Zero, one, or more SQL statements. The query parameters and query fragments continue to go up in number across different
-- statements, e.g. "SELECT $1; SELECT $2; SELECT $3; ...".
-- Use `breakIntoQueryStatements` to get individual SQL statements that are good to send to Postgres.
data Query = Query {queryString :: ![SingleQueryFragment], queryParams :: ![Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)]}

-- | A single statement, not multiple, with dollar-numbered query arguments
-- starting from $1.
data SingleQuery = SingleQuery {queryString :: !ByteString, queryParams :: ![Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)]}

instance Show SingleQuery where
  -- Careful not exposing query arguments
  show (SingleQuery {queryString}) = show queryString

instance Show Query where
  -- Careful not exposing query arguments
  show = mconcat . map show . NE.toList . breakQueryIntoStatements

-- | Takes in a query string with query arguments either as question marks or as dollar-numbered arguments,
-- but not both. Examples:
-- - "SELECT * FROM table WHERE col1=? AND col2=?"
-- - "SELECT * FROM table WHERE col1=$1 AND col2=$2"
-- Note that if the number of arguments does not match what's in the query string,
-- this will throw an error.
mkQuery :: (ToPgRow a) => ByteString -> a -> Query
mkQuery t p = mkQueryInternalFromSqlStatements (parseSql $ decodeUtf8 t) (toPgParams p)

-- | Meant for internal usage, helps build "VALUES (..), (..)"-like statements.
-- Users of hpgsql should just use the `Values` type instead of this.
commaSeparatedRowTuples :: [[Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)]] -> Query
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

breakQueryIntoStatements :: Query -> NonEmpty SingleQuery
breakQueryIntoStatements Query {queryString, queryParams} = toEmptyQueryIfNecessary $ go queryString queryParams
  where
    toEmptyQueryIfNecessary [] = NE.singleton $ SingleQuery {queryString = "", queryParams}
    toEmptyQueryIfNecessary (x : xs) = x :| xs
    isLastFragmentOfAStatement = \case
      FragmentOfStaticSql t -> ";" `BS.isSuffixOf` t -- TODO: A proper ctor for semi-colons separating statements
      _ -> False
    fragToBytestring = \case
      QueryArgumentPlaceHolder n -> "$" <> intToBs n
      FragmentOfStaticSql t -> t
    go :: [SingleQueryFragment] -> [Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)] -> [SingleQuery]
    go [] [] = []
    go [] _ = error "HPgsql error: empty query string but outstanding query params"
    go frags params =
      let (stmtFrags, nextFrags) = case List.break isLastFragmentOfAStatement frags of
            (firstStmts, []) -> (firstStmts, [])
            (firstStmts, semiColon : next) -> (firstStmts ++ [semiColon], next)
          (maxArgNum, thisQueryFrags) = renumberParamsFrom stmtFrags 1
          (thisQueryParams, nextParams) = List.splitAt maxArgNum params
          nextStmts = go nextFrags nextParams
       in SingleQuery {queryString = mconcat $ map fragToBytestring thisQueryFrags, queryParams = thisQueryParams} : nextStmts

intToBs :: Int -> ByteString
intToBs = encodeUtf8 . Text.pack . show

instance IsString Query where
  fromString s =
    -- For a string to be a `Query` all by itself, it means it can't have query
    -- arguments. Users should use the quasiquoter or `mkQuery` for query arguments.
    let statements = parseSql (Text.pack s)
        stmtToSingleQueryFrag :: SqlStatement -> SingleQueryFragment
        stmtToSingleQueryFrag = FragmentOfStaticSql . encodeUtf8 . sqlStatementText
     in Query {queryString = NE.toList $ fmap stmtToSingleQueryFrag statements, queryParams = []}

-- | Takes in a query string with query arguments either as question marks or as dollar-numbered arguments,
-- but not both. Examples:
-- - "SELECT * FROM table WHERE col1=? AND col2=?"
-- - "SELECT * FROM table WHERE col1=$1 AND col2=$2"
-- You should probably use `mkQuery` instead of this. It's here only for hpgsql-simple-compat.
mkQueryInternal :: ByteString -> [Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)] -> Query
mkQueryInternal queryTemplate allParams =
  let statements = parseSql (decodeUtf8 queryTemplate)
   in mkQueryInternalFromSqlStatements statements allParams

mkQueryInternalFromSqlStatements :: NonEmpty SqlStatement -> [Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)] -> Query
mkQueryInternalFromSqlStatements statements allParams =
  let (_, queryFrags) =
        List.mapAccumL
          ( \(!maxArgSoFar) sqlPiece -> case sqlPiece of
              Block t -> (maxArgSoFar, FragmentOfStaticSql $ encodeUtf8 t)
              NotBlock t -> (maxArgSoFar, FragmentOfStaticSql $ encodeUtf8 t)
              DollarNumberedArg n -> (max maxArgSoFar n, QueryArgumentPlaceHolder n)
              QuestionMarkArg -> (maxArgSoFar + 1, QueryArgumentPlaceHolder (maxArgSoFar + 1))
          )
          0
          (concatMap statementBlocks $ NE.toList statements)
   in Query {queryString = queryFrags, queryParams = allParams}

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = liftQueries . parseSql . Text.pack,
      quotePat = error "HPgsql's sql quasiquoter does not implement quotePat",
      quoteType = error "HPgsql's sql quasiquoter does not implement quoteType",
      quoteDec = error "HPgsql's sql quasiquoter does not implement quoteDec"
    }

liftQueries :: NonEmpty SqlStatement -> Q Exp
liftQueries statements = do
  queryExps <- NE.toList <$> mapM liftQuery statements
  case queryExps of
    [] -> error "impossible: NonEmpty produced empty list"
    [single] -> pure single
    (first : rest) -> foldM (\acc q -> [|$(pure acc) <> $(pure q)|]) first rest

liftQuery :: SqlStatement -> Q Exp
liftQuery stmt = do
  let allFragments = extractFragments stmt
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
  let name = mkName (Text.unpack haskellExpr)
   in [|ParamPart (\tyiCache -> (toTypeOid (proxyOf $(varE name)) tyiCache, toPgField tyiCache $(varE name)))|]
fragmentToPartExp (EmbeddedQueryExpr haskellExpr) =
  let name = mkName (Text.unpack haskellExpr)
   in [|EmbeddedQueryPart $(varE name)|]

-- | Walk through fragments in order, building SingleQueryFragment TH expressions
-- and collecting the interpolated Haskell expressions.
buildQueryFragsAndVars :: [SqlFragment] -> Int -> ([Q Exp], [Text])
buildQueryFragsAndVars [] _ = ([], [])
buildQueryFragsAndVars (NonInterpolatedSqlFragment t : rest) n =
  let (restFrags, restVars) = buildQueryFragsAndVars rest n
   in ([|FragmentOfStaticSql (encodeUtf8 $(litE (stringL (Text.unpack t))))|] : restFrags, restVars)
buildQueryFragsAndVars (InterpolatedHaskellExpr var : rest) n =
  let (restFrags, restVars) = buildQueryFragsAndVars rest (n + 1)
   in ([|QueryArgumentPlaceHolder $(litE (integerL (fromIntegral n)))|] : restFrags, var : restVars)
buildQueryFragsAndVars (EmbeddedQueryExpr _ : _) _ =
  error "Bug in HPgsql: EmbeddedQueryExpr should not appear in static path"

-- | Parts used to build a SingleQuery at runtime when ^{} embedded queries are present.
data QueryBuildPartQQ
  = StaticSqlPart !ByteString
  | ParamPart !(Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString))
  | EmbeddedQueryPart !Query

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
      ParamPart p ->
        let (restFrags, restParams) = go rest (argNum + 1)
         in (QueryArgumentPlaceHolder argNum : restFrags, p : restParams)
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

-- | Extract SqlFragment objects from SqlStatement
extractFragments :: SqlStatement -> [SqlFragment]
extractFragments (SqlStatement blocks) = concatMap parseBlock blocks

parseBlock :: BlockOrNotBlock -> [SqlFragment]
parseBlock (NotBlock text) = parseInterpolations text
parseBlock (Block text) = [NonInterpolatedSqlFragment text]
parseBlock (DollarNumberedArg n) = parseBlock $ Block $ "$" <> Text.pack (show n) -- If someone uses e.g. $2 in a quasiquoter, it should be an error, but let's assume they know what they're doing and treat it as text
parseBlock QuestionMarkArg = parseBlock $ Block "?" -- If someone uses a question mark in a quasiquoter, it should be an error, but let's assume they know what they're doing and treat it as text

-- | Parse text to find #{haskellExpr} interpolation patterns and ^{expr} embedded
-- query patterns and return those separated from the rest.
parseInterpolations :: Text -> [SqlFragment]
parseInterpolations txt = case Parsec.parseOnly (Parsec.many' fragmentP <* Parsec.endOfInput) txt of
  Right fragments -> fragments
  Left err -> error $ "Hpgsql error parsing sql quasiquoter: " ++ err
  where
    fragmentP :: Parser SqlFragment
    fragmentP = interpolationP <|> embeddedQueryP <|> literalP

    interpolationP :: Parser SqlFragment
    interpolationP = do
      _ <- Parsec.string "#{"
      -- We hope no Haskell expressions contain '}'
      varName <- Parsec.takeWhile1 (/= '}') <|> fail "Found empty #{} or #{ without closing bracket"
      _ <- Parsec.char '}' <|> fail "Found #{ with an expression but no closing bracket"
      pure $ InterpolatedHaskellExpr varName

    embeddedQueryP :: Parser SqlFragment
    embeddedQueryP = do
      _ <- Parsec.string "^{"
      exprText <- Parsec.takeWhile1 (/= '}') <|> fail "Found empty ^{} or ^{ without closing bracket"
      _ <- Parsec.char '}' <|> fail "Found ^{ with an expression but no closing bracket"
      pure $ EmbeddedQueryExpr exprText

    literalP :: Parser SqlFragment
    literalP = NonInterpolatedSqlFragment . Text.concat <$> some literalChunk
      where
        literalChunk = Parsec.takeWhile1 (\c -> c /= '#' && c /= '^') <|> hashNotFollowedByBrace <|> caretNotFollowedByBrace
        hashNotFollowedByBrace = do
          _ <- Parsec.char '#'
          mc <- Parsec.peekChar
          case mc of
            Just '{' -> fail "#{ found, leaving parsing to interpolationP"
            _ -> pure "#"
        caretNotFollowedByBrace = do
          _ <- Parsec.char '^'
          mc <- Parsec.peekChar
          case mc of
            Just '{' -> fail "^{ found, leaving parsing to embeddedQueryP"
            _ -> pure "^"

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | Generate a parameter expression for a captured variable
generateParamExp :: Text -> Q Exp
generateParamExp (Text.unpack -> haskellExpr) =
  case parseExp haskellExpr of
    Left err -> error $ "Could not parse Haskell expression '" ++ haskellExpr ++ "': " ++ err
    Right expr ->
      [|\tyiCache -> (toTypeOid (proxyOf $(pure expr)) tyiCache, toPgField tyiCache $(pure expr))|]
