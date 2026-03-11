{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPgsql.Query
  ( Query (..), -- We probably shouldn't export this ctor?
    SingleQuery (..), -- Nor this one
    sql, -- Nor this one
    mkQueryWithQuestionMarks,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import HPgsql.Field (ToPgField (..), ToPgRow (..))
import HPgsql.Parsing (BlockOrNotBlock (..), SqlStatement (..), parseSql, sqlStatementText)
import HPgsql.TypeInfo (Oid)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Represents fragments of SQL text that can include interpolated variables
data SqlFragment = LiteralText !Text | InterpolatedVar !String
  deriving stock (Show, Eq)

newtype Query = Query (NonEmpty SingleQuery)
  deriving newtype (Eq, Semigroup, Show)

data SingleQuery = SingleQuery {queryString :: ByteString, queryParams :: [(Maybe Oid, Maybe LBS.ByteString)]}
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
    (x :| xs) -> [|Query ($(return x) NE.:| $(return $ ListE xs))|]

liftQuery :: SqlStatement -> Q Exp
liftQuery stmt = do
  let allFragments = extractFragments stmt
      (sqlString, varNames) = buildSqlAndVars allFragments 1
  paramExps <- mapM generateParamExp varNames
  let paramList = ListE paramExps
  [|SingleQuery (encodeUtf8 $(litE (stringL sqlString))) $(return paramList)|]

-- | Walk through fragments in order, building the SQL string with $N placeholders
-- and collecting the interpolated variable names.
buildSqlAndVars :: [SqlFragment] -> Int -> (String, [String])
buildSqlAndVars [] _ = ("", [])
buildSqlAndVars (LiteralText t : rest) n =
  let (restSql, restVars) = buildSqlAndVars rest n
   in (Text.unpack t ++ restSql, restVars)
buildSqlAndVars (InterpolatedVar var : rest) n =
  let (restSql, restVars) = buildSqlAndVars rest (n + 1)
   in ("$" ++ show n ++ restSql, var : restVars)

-- | Extract SqlFragment objects from SqlStatement
extractFragments :: SqlStatement -> [SqlFragment]
extractFragments (SqlStatement blocks) = concatMap parseBlock blocks

parseBlock :: BlockOrNotBlock -> [SqlFragment]
parseBlock (NotBlock text) = parseInterpolations text
parseBlock (Block text) = [LiteralText text]

-- | Parse text to find #{variableName} interpolation patterns
parseInterpolations :: Text -> [SqlFragment]
parseInterpolations text = go text []
  where
    go txt acc
      | Text.null txt = reverse acc
      | Text.isPrefixOf "#{" txt = case Text.breakOn "}" (Text.drop 2 txt) of
          ("", "") -> reverse acc
          (_, "") -> reverse (LiteralText txt : acc)
          (varName, rest) ->
            let remaining = Text.drop 1 rest
                fragment = InterpolatedVar (Text.unpack varName)
             in if Text.null varName
                  then go remaining (LiteralText "#{" : acc)
                  else go remaining (fragment : acc)
      | otherwise =
          case Text.breakOn "#{" txt of
            (prefix, "") -> reverse (LiteralText prefix : acc)
            (prefix, suffix) -> go suffix (LiteralText prefix : acc)

-- | Generate a parameter expression for a captured variable
generateParamExp :: String -> Q Exp
generateParamExp varName =
  [|(Nothing, toPgField $(varE (mkName varName)))|]
