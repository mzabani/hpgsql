{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPgsql.Query
  ( Query (..), -- We probably shouldn't export this ctor?
    sql,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import HPgsql.Parsing (SqlPiece(..), parseSql, BlockOrNotBlock(..))
import HPgsql.Field (ToPgField(..))
import HPgsql.TypeInfo (Oid)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Proxy (Proxy(..))

-- | Represents fragments of SQL text that can include interpolated variables
data SqlFragment = LiteralText !Text | InterpolatedVar !String
  deriving stock (Show, Eq)

data Query = Query {queryString :: ByteString, queryParams :: [(Maybe Oid, Maybe LBS.ByteString)]}

instance IsString Query where
  fromString s = Query (encodeUtf8 $ Text.pack s) []

parseQuery :: String -> Either String [SqlPiece]
parseQuery q =
  let sqlPieces = parseSql q
   in Right sqlPieces

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = \s ->
        case parseQuery s of
          Left err -> fail err
          Right q -> liftQuery q,
      quotePat = error "HPgsql's sql quasiquoter does not implement quotePat",
      quoteType = error "HPgsql's sql quasiquoter does not implement quoteType",
      quoteDec = error "HPgsql's sql quasiquoter does not implement quoteDec"
    }

liftQuery :: [SqlPiece] -> Q Exp
liftQuery pieces = do
  -- Extract fragments from SqlPiece, focusing on OtherSqlPiece
  let allFragments = concatMap extractFragments pieces
  
  -- Separate literal text from interpolated variables
  let (literals, interpolatedVars) = partitionFragments allFragments
  
  -- Generate parameter placeholders ($1, $2, etc.)
  let paramCount = length interpolatedVars
      placeholders = map (\i -> "$" ++ show i) [1..paramCount]
  
  -- Build the final SQL string by replacing interpolations with placeholders
  let sqlString = buildSqlString literals interpolatedVars placeholders
  
  -- Generate parameter expressions with proper type information
  paramExps <- mapM generateParamExp interpolatedVars
  
  -- Return the complete Query constructor
  let paramList = ListE paramExps
  [| Query (encodeUtf8 $(litE (stringL sqlString))) $(return paramList) |]

-- | Extract SqlFragment objects from SqlPiece, focusing on OtherSqlPiece
extractFragments :: SqlPiece -> [SqlFragment]
extractFragments (OtherSqlPiece blocks) = concatMap parseBlock blocks
extractFragments _ = []

-- | Parse a BlockOrNotBlock into SqlFragments, detecting interpolations
parseBlock :: BlockOrNotBlock -> [SqlFragment]
parseBlock (NotBlock text) = parseInterpolations text
parseBlock (Block text) = [LiteralText text]  -- Don't interpolate inside blocks

-- | Parse text to find #{variableName} interpolation patterns
parseInterpolations :: Text -> [SqlFragment]
parseInterpolations text = go text []
  where
    go txt acc
      | Text.null txt = reverse acc
      | Text.isPrefixOf "#{" txt = case Text.breakOn "}" (Text.drop 2 txt) of
          ("", "") -> reverse acc  -- Empty variable name, treat as literal
          (varName, "") -> reverse (LiteralText txt : acc)  -- No closing brace, treat as literal
          (varName, rest) -> 
            let remaining = Text.drop 1 rest  -- Drop the closing brace
                fragment = InterpolatedVar (Text.unpack varName)
            in if Text.null varName 
               then go remaining (LiteralText "#{" : acc)  -- Empty variable name
               else go remaining (fragment : acc)
      | otherwise = 
          case Text.breakOn "#{" txt of
            (prefix, "") -> reverse (LiteralText prefix : acc)  -- No more interpolations
            (prefix, suffix) -> go suffix (LiteralText prefix : acc)

-- | Partition fragments into literals and variable names
partitionFragments :: [SqlFragment] -> ([Text], [String])
partitionFragments = go [] []
  where
    go literals vars [] = (reverse literals, reverse vars)
    go literals vars (LiteralText t : rest) = go (t : literals) vars rest
    go literals vars (InterpolatedVar var : rest) = go literals (var : vars) rest

-- | Build the final SQL string by replacing interpolations with placeholders
buildSqlString :: [Text] -> [String] -> [String] -> String
buildSqlString literals varNames placeholders = 
  let combined = interleaveTextAndPlaceholders literals placeholders
  in Text.unpack $ Text.concat combined
  where
    interleaveTextAndPlaceholders [] [] = []
    interleaveTextAndPlaceholders (t:ts) [] = t : ts  -- More literals than placeholders
    interleaveTextAndPlaceholders [] (p:ps) = [Text.pack p]  -- More placeholders than literals (shouldn't happen)
    interleaveTextAndPlaceholders (t:ts) (p:ps) = t : Text.pack p : interleaveTextAndPlaceholders ts ps

-- | Generate a parameter expression for a captured variable
generateParamExp :: String -> Q Exp
generateParamExp varName = do
  -- Generate a simple parameter expression without type inference for now
  -- We'll use a generic approach that works for basic types
  [| (Nothing, toPgField $(varE (mkName varName))) |]
