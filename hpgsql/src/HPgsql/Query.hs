{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPgsql.Query
  ( Query (..), -- We probably shouldn't export this ctor?
    SingleQuery (..), -- Nor this one
    sql,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import HPgsql.Field (ToPgField (..))
import HPgsql.Parsing (BlockOrNotBlock (..), SqlPiece (..), parseSql, piecesToText)
import HPgsql.TypeInfo (Oid)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Represents fragments of SQL text that can include interpolated variables
data SqlFragment = LiteralText !Text | InterpolatedVar !String
  deriving stock (Show, Eq)

newtype Query = Query (NonEmpty SingleQuery)

data SingleQuery = SingleQuery {queryString :: ByteString, queryParams :: [(Maybe Oid, Maybe LBS.ByteString)]}

instance IsString Query where
  fromString s =
    let statements = parseSql s
     in Query $ fmap (\pieces -> SingleQuery (encodeUtf8 $ piecesToText pieces) []) statements

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = liftQueries . parseSql,
      quotePat = error "HPgsql's sql quasiquoter does not implement quotePat",
      quoteType = error "HPgsql's sql quasiquoter does not implement quoteType",
      quoteDec = error "HPgsql's sql quasiquoter does not implement quoteDec"
    }

liftQueries :: NonEmpty [SqlPiece] -> Q Exp
liftQueries statements = do
  queryExps <- mapM liftQuery statements
  case queryExps of
    (x :| xs) -> [|Query ($(return x) NE.:| $(return $ ListE xs))|]

liftQuery :: [SqlPiece] -> Q Exp
liftQuery pieces = do
  let allFragments = concatMap extractFragments pieces
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

-- | Extract SqlFragment objects from SqlPiece
extractFragments :: SqlPiece -> [SqlFragment]
extractFragments (SqlStatementPiece blocks) = concatMap parseBlock blocks
extractFragments _ = []

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
