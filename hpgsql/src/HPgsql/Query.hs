{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPgsql.Query
  ( Query (..), -- We probably shouldn't export this ctor?
    SingleQuery (..), -- Nor this one
    sql, -- Nor this one
    mkQueryWithQuestionMarks,
    QueryBuildPart (..),
    buildSingleQuery,
  )
where

import Control.Applicative (some, (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import HPgsql.Field (ToPgField (..), ToPgRow (..))
import HPgsql.Parsing (BlockOrNotBlock (..), SqlStatement (..), parseSql, sqlStatementText)
import HPgsql.TypeInfo (Oid)
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Represents fragments of SQL text that can include interpolated Haskell expressions,
-- embedded Query expressions, or fragments of plain SQL.
data SqlFragment
  = NonInterpolatedSqlFragment !Text
  | InterpolatedHaskellExpr !Text
  | -- | @^{expr}@ where @expr :: Query@
    EmbeddedQueryExpr !Text
  deriving stock (Eq)

newtype Query = Query (NonEmpty SingleQuery)
  deriving newtype (Eq, Semigroup, Show)

data SingleQuery = SingleQuery {queryString :: ByteString, queryParams :: [(Maybe Oid, Maybe LBS.ByteString)]}
  -- Not an auto-derived Semigroup because concatenation would require re-indexing query arguments
  deriving stock (Eq)

instance Show SingleQuery where
  -- Careful not exposing query arguments
  show (SingleQuery {queryString}) = show queryString

instance IsString Query where
  fromString s =
    let statements = parseSql (Text.pack s)
     in Query $ fmap (\stmt -> SingleQuery (encodeUtf8 $ sqlStatementText stmt) []) statements

-- | Takes in a query string with question marks as placeholders for query arguments,
-- e.g. "SELECT * FROM table WHERE col1=? AND col2=?", and a row object, and returns
-- the equivalent query for Hpgsql to run.
mkQueryWithQuestionMarks :: (ToPgRow r) => ByteString -> r -> Query
mkQueryWithQuestionMarks queryTemplate r =
  let allParams = toPgParams r
      statements = parseSql (decodeUtf8 queryTemplate)
   in Query $ distributeParams allParams statements
  where
    processStatement :: SqlStatement -> (ByteString, Int)
    processStatement (SqlStatement blocks) =
      let (finalN, textParts) = List.foldl' processBlock (1, []) blocks
       in (encodeUtf8 $ Text.concat (reverse textParts), finalN - 1)

    processBlock :: (Int, [Text]) -> BlockOrNotBlock -> (Int, [Text])
    processBlock (n, acc) (Block t) = (n, t : acc)
    processBlock (n, acc) (NotBlock t) =
      let (n', t') = replaceQuestionMarks n t
       in (n', t' : acc)

    replaceQuestionMarks :: Int -> Text -> (Int, Text)
    replaceQuestionMarks n txt =
      let (before, rest) = Text.break (== '?') txt
       in if Text.null rest
            then (n, before)
            else
              let (n', rest') = replaceQuestionMarks (n + 1) (Text.drop 1 rest)
               in (n', before <> "$" <> Text.pack (show n) <> rest')

    distributeParams :: [(Maybe Oid, Maybe LBS.ByteString)] -> NonEmpty SqlStatement -> NonEmpty SingleQuery
    distributeParams params (stmt :| rest) =
      let (queryBS, numParams) = processStatement stmt
          (theseParams, remainingParams) = splitAt numParams params
          thisQuery = SingleQuery queryBS theseParams
       in case rest of
            [] -> thisQuery :| []
            (s : ss) -> thisQuery NE.<| distributeParams remainingParams (s :| ss)

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
  queryExps <- mapM liftQuery statements
  case queryExps of
    (x :| xs) -> [|Query ($(pure x) NE.:| $(pure $ ListE xs))|]

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
  let (sqlText, varNames) = buildSqlAndVars allFragments 1
  paramExps <- mapM generateParamExp varNames
  let paramList = ListE paramExps
  [|SingleQuery (encodeUtf8 $(litE (stringL (Text.unpack sqlText)))) $(pure paramList)|]

-- | Dynamic path: has ^{} expressions, build query at runtime via buildSingleQuery.
liftQueryDynamic :: [SqlFragment] -> Q Exp
liftQueryDynamic allFragments = do
  partExps <- mapM fragmentToPartExp allFragments
  [|buildSingleQuery $(pure $ ListE partExps)|]

-- | Convert a SqlFragment into a TH expression producing a QueryBuildPart.
fragmentToPartExp :: SqlFragment -> Q Exp
fragmentToPartExp (NonInterpolatedSqlFragment t) =
  [|StaticSqlPart $(litE (stringL (Text.unpack t)))|]
fragmentToPartExp (InterpolatedHaskellExpr haskellExpr) =
  let name = mkName (Text.unpack haskellExpr)
   in [|ParamPart (toTypeOid (proxyOf $(varE name)), toPgField $(varE name))|]
fragmentToPartExp (EmbeddedQueryExpr haskellExpr) =
  let name = mkName (Text.unpack haskellExpr)
   in [|EmbeddedQueryPart $(varE name)|]

-- | Walk through fragments in order, building the SQL string with $N placeholders
-- and collecting the interpolated Haskell expressions.
buildSqlAndVars :: [SqlFragment] -> Int -> (Text, [Text])
buildSqlAndVars [] _ = ("", [])
buildSqlAndVars (NonInterpolatedSqlFragment t : rest) n =
  let (restSql, restVars) = buildSqlAndVars rest n
   in (t <> restSql, restVars)
buildSqlAndVars (InterpolatedHaskellExpr var : rest) n =
  let (restSql, restVars) = buildSqlAndVars rest (n + 1)
   in ("$" <> Text.pack (show n) <> restSql, var : restVars)
buildSqlAndVars (EmbeddedQueryExpr _ : _) _ =
  error "Bug in HPgsql: EmbeddedQueryExpr should not appear in static path"

-- | Parts used to build a SingleQuery at runtime when ^{} embedded queries are present.
data QueryBuildPart
  = StaticSqlPart !ByteString
  | ParamPart !(Maybe Oid, Maybe LBS.ByteString)
  | EmbeddedQueryPart !Query

-- | Build a SingleQuery from a list of QueryBuildPart, renumbering parameters as needed.
buildSingleQuery :: [QueryBuildPart] -> SingleQuery
buildSingleQuery parts =
  let (finalSql, finalParams, _) = List.foldl' processPart (mempty, [], 1 :: Int) parts
   in SingleQuery (encodeUtf8 finalSql) (reverse finalParams)
  where
    processPart :: (Text, [(Maybe Oid, Maybe LBS.ByteString)], Int) -> QueryBuildPart -> (Text, [(Maybe Oid, Maybe LBS.ByteString)], Int)
    processPart (sql', params, n) (StaticSqlPart bs) =
      (sql' <> decodeUtf8 bs, params, n)
    processPart (sql', params, n) (ParamPart param) =
      (sql' <> "$" <> Text.pack (show n), param : params, n + 1)
    processPart (sql', params, n) (EmbeddedQueryPart (Query singleQueries)) =
      case NE.toList singleQueries of
        [SingleQuery innerSql innerParams] ->
          let offset = n - 1
              renumbered = if offset == 0 then decodeUtf8 innerSql else renumberParams offset (decodeUtf8 innerSql)
           in (sql' <> renumbered, reverse innerParams ++ params, n + length innerParams)
        _ -> error "Embedded query (^{}) must contain exactly one SQL statement, but it contains multiple statements separated by semicolons"

-- | Renumber @$N@ placeholders in SQL text by adding an offset to each N.
renumberParams :: Int -> Text -> Text
renumberParams 0 t = t
renumberParams offset t = go t
  where
    go txt =
      let (before, rest) = Text.break (== '$') txt
       in if Text.null rest
            then before
            else
              let afterDollar = Text.drop 1 rest
                  (digits, afterDigits) = Text.span isDigit afterDollar
               in if Text.null digits
                    then before <> "$" <> go afterDigits
                    else
                      let origNum = read (Text.unpack digits) :: Int
                          newNum = origNum + offset
                       in before <> "$" <> Text.pack (show newNum) <> go afterDigits

-- | Extract SqlFragment objects from SqlStatement
extractFragments :: SqlStatement -> [SqlFragment]
extractFragments (SqlStatement blocks) = concatMap parseBlock blocks

parseBlock :: BlockOrNotBlock -> [SqlFragment]
parseBlock (NotBlock text) = parseInterpolations text
parseBlock (Block text) = [NonInterpolatedSqlFragment text]

-- | Parse text to find #{haskellExpr} interpolation patterns and ^{expr} embedded
-- query patterns and return those separated from the rest.
parseInterpolations :: Text -> [SqlFragment]
parseInterpolations txt = case A.parseOnly (A.many' fragmentP <* A.endOfInput) txt of
  Right fragments -> fragments
  Left err -> error $ "Hpgsql error parsing sql quasiquoter: " ++ err
  where
    fragmentP :: Parser SqlFragment
    fragmentP = interpolationP <|> embeddedQueryP <|> literalP

    interpolationP :: Parser SqlFragment
    interpolationP = do
      _ <- A.string "#{"
      -- We hope no Haskell expressions contain '}'
      varName <- A.takeWhile1 (/= '}') <|> fail "Found empty #{} or #{ without closing bracket"
      _ <- A.char '}' <|> fail "Found #{ with an expression but no closing bracket"
      pure $ InterpolatedHaskellExpr varName

    embeddedQueryP :: Parser SqlFragment
    embeddedQueryP = do
      _ <- A.string "^{"
      exprText <- A.takeWhile1 (/= '}') <|> fail "Found empty ^{} or ^{ without closing bracket"
      _ <- A.char '}' <|> fail "Found ^{ with an expression but no closing bracket"
      pure $ EmbeddedQueryExpr exprText

    literalP :: Parser SqlFragment
    literalP = NonInterpolatedSqlFragment . Text.concat <$> some literalChunk
      where
        literalChunk = A.takeWhile1 (\c -> c /= '#' && c /= '^') <|> hashNotFollowedByBrace <|> caretNotFollowedByBrace
        hashNotFollowedByBrace = do
          _ <- A.char '#'
          mc <- A.peekChar
          case mc of
            Just '{' -> fail "#{ found, leaving parsing to interpolationP"
            _ -> pure "#"
        caretNotFollowedByBrace = do
          _ <- A.char '^'
          mc <- A.peekChar
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
      [|(toTypeOid (proxyOf $(pure expr)), toPgField $(pure expr))|]
