{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SqlQuasiquoterSpec where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import HPgsql (Only (..))
import HPgsql.Parsing (parseSql, sqlStatementText)
import HPgsql.Query (Query (..), SingleQuery (..), breakQueryIntoStatements, mkQuery, sql)
import HPgsql.TypeInfo (builtinPgTypesMap)
import Hedgehog (Gen, PropertyT, annotateShow, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "Query concatenation" $ do
    it
      "Concatenating mkQuery queries"
      mkQueryConcatenation
    it
      "Concatenating #{}-interpolated queries"
      interpolationConcatenation
    it
      "Concatenating ^{}-embedded queries"
      embeddedQueryConcatenation
    it
      "Concatenating mixed mkQuery, #{}, and ^{} queries"
      mixedConcatenation

-- | Shared property: concatenating arbitrary queries produces valid results.
checkQueryConcatenation :: Gen Query -> PropertyT IO ()
checkQueryConcatenation gen = hedgehog $ do
  queries <- forAll $ Gen.list (Range.linear 1 50) gen
  let concatenated = foldr1 (<>) queries
      singleQueries = NE.toList $ breakQueryIntoStatements concatenated

  -- Total number of SingleQueries matches the number of generated queries
  length singleQueries === length queries

  -- Each SingleQuery must be independently valid
  forM_ (zip queries singleQueries) $ \(Query {queryParams = expectedParams}, SingleQuery qStr qParams) -> do
    let qText = decodeUtf8 qStr
    annotateShow qText

    -- Re-parsing the query string produces exactly one statement with matching text
    let reparsed = parseSql qText
    length reparsed === 1
    sqlStatementText (NE.head reparsed) === qText

    -- Query arguments must have been distributed correctly
    map ($ builtinPgTypesMap) expectedParams === map ($ builtinPgTypesMap) qParams

    -- \$N placeholders start from 1 and are contiguous, matching param count
    let placeholders = extractPlaceholders qText
        numParams = length qParams

    if numParams == 0
      then placeholders === []
      else placeholders === [1 .. numParams]

mkQueryConcatenation :: PropertyT IO ()
mkQueryConcatenation = checkQueryConcatenation genMkQuery

interpolationConcatenation :: PropertyT IO ()
interpolationConcatenation = checkQueryConcatenation genInterpolatedQuery

embeddedQueryConcatenation :: PropertyT IO ()
embeddedQueryConcatenation = checkQueryConcatenation genEmbeddedQuery

mixedConcatenation :: PropertyT IO ()
mixedConcatenation = checkQueryConcatenation genMixedQuery

-- Generators --

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 minBound maxBound)

-- | Queries built with mkQuery, including reused $N placeholders.
genMkQuery :: Gen Query
genMkQuery =
  Gen.choice
    [ do
        tmpl <- Gen.element noParamTemplates
        pure $ mkQuery tmpl (),
      do
        tmpl <- Gen.element oneParamTemplates
        a <- genInt
        pure $ mkQuery tmpl (Only a),
      do
        tmpl <- Gen.element twoParamTemplates
        a <- genInt
        b <- genInt
        pure $ mkQuery tmpl (a, b),
      do
        tmpl <- Gen.element threeParamTemplates
        a <- genInt
        b <- genInt
        c <- genInt
        pure $ mkQuery tmpl (a, b, c),
      do
        a <- genInt
        b <- genInt
        c <- genInt
        d <- genInt
        pure $ mkQuery "SELECT $1, $2, $3, $4;" (a, b, c, d),
      do
        a <- genInt
        b <- genInt
        c <- genInt
        d <- genInt
        e <- genInt
        pure $ mkQuery "SELECT $1, $2, $3, $4, $5;" (a, b, c, d, e)
    ]

-- | Queries built with the sql quasiquoter and #{} interpolation.
genInterpolatedQuery :: Gen Query
genInterpolatedQuery =
  Gen.choice
    [ pure [sql|SELECT 1;|],
      do
        x <- genInt
        pure [sql|SELECT #{x};|],
      do
        x <- genInt
        y <- genInt
        pure [sql|SELECT #{x}, #{y};|],
      do
        x <- genInt
        y <- genInt
        z <- genInt
        pure [sql|SELECT #{x} FROM t WHERE #{y} BETWEEN 0 AND #{z};|]
    ]

-- | Queries built with ^{} embedded queries, including reused placeholders.
genEmbeddedQuery :: Gen Query
genEmbeddedQuery =
  Gen.choice
    [ do
        x <- genInt
        let inner = mkQuery "$1" (Only x)
        pure [sql|SELECT ^{inner};|],
      do
        x <- genInt
        let inner = mkQuery "$1 + $1" (Only x)
        pure [sql|SELECT ^{inner};|],
      do
        x <- genInt
        y <- genInt
        let inner = mkQuery "$1, $2" (x, y)
        pure [sql|SELECT ^{inner};|],
      do
        x <- genInt
        y <- genInt
        let inner1 = mkQuery "$1" (Only x)
            inner2 = mkQuery "$1" (Only y)
        pure [sql|SELECT ^{inner1}, ^{inner2};|],
      do
        x <- genInt
        y <- genInt
        z <- genInt
        let inner1 = mkQuery "$1, $2" (x, y)
            inner2 = mkQuery "$1" (Only z)
        pure [sql|SELECT ^{inner1}, ^{inner2};|],
      do
        x <- genInt
        y <- genInt
        z <- genInt
        let inner1 = mkQuery "$1" (Only x)
            inner2 = mkQuery "$1 + $1, $2" (y, z)
        pure [sql|SELECT ^{inner1}, ^{inner2};|]
    ]

-- | Mix of mkQuery, #{} interpolation, and ^{} embedding.
genMixedQuery :: Gen Query
genMixedQuery =
  Gen.choice
    [ genMkQuery,
      genInterpolatedQuery,
      genEmbeddedQuery,
      do
        x <- genInt
        y <- genInt
        let inner = mkQuery "$1" (Only y)
        pure [sql|SELECT #{x}, ^{inner};|],
      do
        x <- genInt
        y <- genInt
        let inner = mkQuery "$1 + $1" (Only y)
        pure [sql|SELECT #{x}, ^{inner};|],
      do
        x <- genInt
        y <- genInt
        z <- genInt
        w <- genInt
        let inner = mkQuery "$1, $2" (y, z)
        pure [sql|SELECT #{x}, ^{inner}, #{w};|],
      do
        x <- genInt
        y <- genInt
        z <- genInt
        let inner1 = mkQuery "$1" (Only y)
            inner2 = mkQuery "$1" (Only z)
        pure [sql|SELECT #{x}, ^{inner1}, ^{inner2};|]
    ]

-- Templates --

noParamTemplates :: [ByteString]
noParamTemplates =
  [ "SELECT 1;",
    "SELECT TRUE, FALSE;",
    "SELECT 'hello';",
    "SELECT 1, 2, 3;"
  ]

oneParamTemplates :: [ByteString]
oneParamTemplates =
  [ "SELECT $1;",
    "SELECT $1 FROM generate_series(1, 10);",
    "SELECT $1 WHERE $1 > 0;",
    "SELECT $1, $1 + 1;"
  ]

twoParamTemplates :: [ByteString]
twoParamTemplates =
  [ "SELECT $1, $2;",
    "SELECT $1 FROM t WHERE x = $2;",
    "SELECT $1, $2, $1 + $2;",
    "SELECT COALESCE($1, $2), COALESCE($2, $1);"
  ]

threeParamTemplates :: [ByteString]
threeParamTemplates =
  [ "SELECT $1, $2, $3;",
    "SELECT $1 FROM t WHERE x BETWEEN $2 AND $3;",
    "SELECT $1 + $2 + $3, $3 - $2 - $1;",
    "SELECT $1 WHERE $2 = $3 AND $1 = $2;"
  ]

-- | Extract all $N placeholder numbers from SQL text, returning them sorted and deduplicated.
-- This is a different implementation from the application, and it is so on purpose.
extractPlaceholders :: Text -> [Int]
extractPlaceholders = List.sort . List.nub . go
  where
    go txt =
      case Text.break (== '$') txt of
        (_, rest)
          | Text.null rest -> []
          | otherwise ->
              let afterDollar = Text.drop 1 rest
                  (digits, remaining) = Text.span isDigit afterDollar
               in if Text.null digits
                    then go afterDollar
                    else read (Text.unpack digits) : go remaining
