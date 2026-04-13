{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SqlQuasiquoterSpec where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import HPgsql (Only (..))
import HPgsql.Encoding (ToPgRow (..))
import HPgsql.Parsing (ParsingOpts (..), parseSql, sqlStatementText)
import HPgsql.Query (Query (..), SingleQuery (..), breakQueryIntoStatements, mkQuery, sql)
import HPgsql.TypeInfo (EncodingContext (..), Oid, builtinPgTypesMap)
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
    it "Parenthesized Haskell expressions in ^{} work" $ do
      let x = 42 :: Int
          inner = mkQuery "$1" (Only x)
          query = getUniqueNE [sql|SELECT ^{(inner)};|]
      query.queryString `shouldBe` encodeUtf8 "SELECT $1;"
      length query.queryParams `shouldBe` 1

-- | Shared property: concatenating arbitrary queries produces valid results.
checkQueryConcatenation :: Gen (Query, [(Maybe Oid, Maybe LBS.ByteString)]) -> PropertyT IO ()
checkQueryConcatenation gen = hedgehog $ do
  queries <- forAll $ Gen.list (Range.linear 1 50) gen
  let concatenated = foldr1 (<>) $ map fst queries
      singleQueries = NE.toList $ breakQueryIntoStatements concatenated

  -- Total number of SingleQueries matches the number of generated queries
  length singleQueries === length queries

  -- Each SingleQuery must be independently valid
  forM_ (zip queries singleQueries) $ \((_, expectedParams), SingleQuery qStr qParams) -> do
    let qText = decodeUtf8 qStr
    annotateShow qText

    -- Re-parsing the query string produces exactly one statement with matching text
    let reparsed = parseSql AcceptOnlyDollarNumberedArgs qText
    length reparsed === 1
    sqlStatementText (NE.head reparsed) === qText

    -- Query arguments must have been distributed correctly
    expectedParams === map ($ EncodingContext builtinPgTypesMap) qParams

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

genChar :: Gen Char
genChar = Gen.enum 'a' 'z'

genString :: Gen String
genString = Gen.string (Range.linearFrom 0 (-10) 10) genChar

toComparableParams :: (ToPgRow a) => a -> [(Maybe Oid, Maybe LBS.ByteString)]
toComparableParams = map ($ EncodingContext builtinPgTypesMap) . toPgParams

-- | Queries built with mkQuery, including reused $N placeholders.
genMkQuery :: Gen (Query, [(Maybe Oid, Maybe LBS.ByteString)])
genMkQuery =
  Gen.choice
    [ do
        tmpl <- Gen.element noParamTemplates
        pure (mkQuery tmpl (), []),
      do
        tmpl <- Gen.element oneParamTemplates
        a <- genInt
        let params = Only a
        pure (mkQuery tmpl params, toComparableParams params),
      do
        tmpl <- Gen.element twoParamTemplates
        a <- genInt
        b <- genInt
        let params = (a, b)
        pure (mkQuery tmpl params, toComparableParams params),
      do
        tmpl <- Gen.element threeParamTemplates
        a <- genInt
        b <- genInt
        c <- genInt
        let params = (a, b, c)
        pure (mkQuery tmpl params, toComparableParams params),
      do
        a <- genInt
        b <- genInt
        c <- genInt
        d <- genInt
        let params = (a, b, c, d)
        pure (mkQuery "SELECT $1, $2, $3, $4;" params, toComparableParams params),
      do
        a <- genInt
        b <- genInt
        c <- genInt
        d <- genInt
        e <- genInt
        let params = (a, b, c, d, e)
        pure (mkQuery "SELECT $1, $2, $3, $4, $5;" params, toComparableParams params)
    ]

-- | Queries built with the sql quasiquoter and #{} interpolation.
genInterpolatedQuery :: Gen (Query, [(Maybe Oid, Maybe LBS.ByteString)])
genInterpolatedQuery =
  Gen.choice
    [ pure ([sql|SELECT 1, '#{x}', '^{y}';|], []),
      do
        x <- genInt
        pure ([sql|SELECT #{x};|], toComparableParams (Only x)),
      do
        x <- genInt
        y <- genInt
        pure ([sql|SELECT #{x}, #{y};|], toComparableParams (x, y)),
      do
        x <- genInt
        y <- genInt
        z <- genInt
        pure ([sql|SELECT #{x} FROM t WHERE #{y} BETWEEN 0 AND #{z};|], toComparableParams (x, y, z))
    ]

-- | Queries built with ^{} embedded queries, including reused placeholders.
genEmbeddedQuery :: Gen (Query, [(Maybe Oid, Maybe LBS.ByteString)])
genEmbeddedQuery =
  Gen.choice
    [ do
        x <- genInt
        let inner = mkQuery "$1" (Only x)
        pure ([sql|SELECT ^{inner};|], toComparableParams (Only x)),
      do
        x <- genInt
        let inner = mkQuery "$1 + $1" (Only x)
        pure ([sql|SELECT ^{inner};|], toComparableParams (Only x)),
      do
        x <- genInt
        y <- genInt
        let inner = mkQuery "$1, $2" (x, y)
        pure ([sql|SELECT ^{inner};|], toComparableParams (x, y)),
      do
        x <- genInt
        y <- genInt
        let inner1 = mkQuery "$1" (Only x)
            inner2 = mkQuery "$1" (Only y)
        pure ([sql|SELECT ^{inner1}, ^{inner2};|], toComparableParams (x, y)),
      do
        x <- genInt
        y <- genInt
        z <- genInt
        let inner1 = mkQuery "$1, $2" (x, y)
            inner2 = mkQuery "$1" (Only z)
        pure ([sql|SELECT ^{inner1}, ^{inner2};|], toComparableParams (x, y, z)),
      do
        x <- genInt
        y <- genInt
        z <- genInt
        let inner1 = mkQuery "$1" (Only x)
            inner2 = mkQuery "$1 + $1, $2" (y, z)
        pure ([sql|SELECT ^{inner1}, ^{inner2};|], toComparableParams (x, y, z))
    ]

-- | Mix of mkQuery, #{} interpolation, and ^{} embedding.
genMixedQuery :: Gen (Query, [(Maybe Oid, Maybe LBS.ByteString)])
genMixedQuery =
  Gen.choice
    [ genMkQuery,
      genInterpolatedQuery,
      genEmbeddedQuery,
      do
        x <- genInt
        y <- genInt
        let inner = mkQuery "$1" (Only y)
        pure ([sql|SELECT #{x}, ^{inner};|], toComparableParams (x, y)),
      do
        x <- genInt
        y <- genInt
        let inner = mkQuery "$1 + $1" (Only y)
        pure ([sql|SELECT #{x + (x + 1)}, ^{inner};|], toComparableParams (x + (x + 1), y)),
      do
        x <- genInt
        y <- genInt
        z <- genInt
        w <- genString
        let inner = mkQuery "$1, $2" (y, z)
        pure ([sql|SELECT #{x}, '#{x}', ^{inner}, (#{"abc" ++ w});|], toComparableParams (x, y, z, "abc" ++ w)),
      do
        x <- genInt
        y <- genInt
        z <- genInt
        let inner1 = mkQuery "$1" (Only y)
            inner2 = mkQuery "$1" (Only z)
        pure ([sql|SELECT '#{x}', '^{inner1}', (#{x}), (^{inner1}), ^{inner2};|], toComparableParams (x, y, z))
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
    "SELECT $1, $1 + 1;",
    "SELECT '{\"a\":1, \"b\":2}'::jsonb ? $1;"
  ]

twoParamTemplates :: [ByteString]
twoParamTemplates =
  [ "SELECT $1, $2;",
    "SELECT $1 FROM t WHERE x = $2;",
    "SELECT $1, $2, $1 + $2;",
    "SELECT COALESCE($1, $2), COALESCE($2, $1);",
    "SELECT $1::jsonb ?| $2;"
  ]

threeParamTemplates :: [ByteString]
threeParamTemplates =
  [ "SELECT $1, $2, $3;",
    "SELECT $1 FROM t WHERE x BETWEEN $2 AND $3;",
    "SELECT $1 + $2 + $3, $3 - $2 - $1;",
    "SELECT $1 WHERE $2 = $3 AND $1 = $2;"
  ]

getUniqueNE :: (HasCallStack) => Query -> SingleQuery
getUniqueNE = NE.head . breakQueryIntoStatements

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
