module SqlQuasiquoterSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Hedgehog (Gen, PropertyT, annotateShow, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hpgsql.Builder (BinaryField (..))
import Hpgsql.Encoding (FieldEncoder (..), FromPgField, LowerCasedPgEnum (..), RowEncoder (..), ToPgField (..), ToPgRow (..), compositeTypeEncoder, typeFieldEncoder, typeOidWithName)
import Hpgsql.Internal.InternalTypes (Query (..), SingleQuery (..), queryToByteString)
import Hpgsql.ParsingInternal (ParsingOpts (..), parseSql)
import Hpgsql.Query (breakQueryIntoStatements, escapeIdentifier, mkQuery, sql, vALUES)
import Hpgsql.TypeInfo (EncodingContext (..), Oid, builtinPgTypesMap)
import Hpgsql.Types (Only (..))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = parallel $ do
  describe "Query concatenation" $ do
    it
      "Concatenating fragments of one single statement"
      fragmentsSingleStatementConcatenation
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

  describe "Explicit validation errors" $ do
    it "mkQuery throws when a dollar-numbered placeholder is less than 1" $ do
      evaluate (force $ show (mkQuery "SELECT $0" (Only (1 :: Int))).queryString) `shouldThrow` anyErrorCall

    it "mkQuery throws when a dollar-numbered placeholder exceeds the supplied argument count" $ do
      evaluate (force $ show (mkQuery "SELECT $2" (Only (1 :: Int))).queryString) `shouldThrow` anyErrorCall

    it "breakQueryIntoStatements throws promptly when there are more supplied arguments than referenced placeholders" $ do
      let q = mkQuery "SELECT $1" (1 :: Int, 2 :: Int)
      evaluate (length (NE.toList (breakQueryIntoStatements q))) `shouldThrow` anyErrorCall

-- | Concatenating arbitrary queries produces valid results.
checkQueryConcatenation :: Gen (Query, [(Maybe Oid, BinaryField)]) -> PropertyT IO ()
checkQueryConcatenation gen = hedgehog $ do
  queries <- forAll $ Gen.list (Range.linear 1 50) gen
  let concatenated = foldr1 (<>) $ map fst queries
      singleQueries = NE.toList $ breakQueryIntoStatements concatenated

  -- Total number of SingleQueries matches the number of generated queries
  annotateShow singleQueries
  length singleQueries === length queries

  -- Each SingleQuery must be independently valid
  forM_ (zip queries singleQueries) $ \((_, expectedParams), SingleQuery qStr qParams _isPrepared) -> do
    let qText = decodeUtf8 qStr
    annotateShow qText

    -- -- Re-parsing the query string produces exactly one statement with matching text
    -- let reparsed = parseSql AcceptOnlyDollarNumberedArgs qText
    -- length reparsed === 1
    -- sqlStatementText (NE.head reparsed) === qText

    -- Query arguments must have been distributed correctly
    expectedParams === map ($ EncodingContext builtinPgTypesMap) qParams

    -- \$N placeholders start from 1 and are contiguous, matching param count
    let placeholders = extractPlaceholders qText
        numParams = length qParams

    placeholders === [1 .. numParams]

-- | Concatenating fragments that together form a single SQL statement produces
-- valid parameter interpolation and a valid query string.
fragmentsSingleStatementConcatenation :: PropertyT IO ()
fragmentsSingleStatementConcatenation = hedgehog $ do
  fragments :: [(ByteString, (Int, Int))] <-
    forAll $
      Gen.list (Range.linear 1 100) $
        Gen.element
          [ ("$1, $2 ", (1, 37)),
            ("$1 FROM t WHERE x = $2 ", (91, 15)),
            ("$1, $2, $1 + $2,", (49, 61)),
            ("COALESCE($1, $2), COALESCE($2, $1),", ((-38), 42)),
            ("($1::jsonb ?| $2),", (0, 1237))
          ]
  let concatenatedQuery = mconcat $ map (\(queryString, queryArgs) -> mkQuery queryString queryArgs) fragments
      expectedParams :: [Int] = concatMap (\(_, (arg1, arg2)) -> [arg1, arg2]) fragments
      encCtx = EncodingContext builtinPgTypesMap
      intEncoder = fieldEncoder @Int
  map ($ encCtx) concatenatedQuery.queryParams === map (\argValue -> (intEncoder.toTypeOid encCtx, intEncoder.toPgField encCtx argValue)) expectedParams
  length (breakQueryIntoStatements concatenatedQuery) === 1
  extractPlaceholders (decodeUtf8 (queryToByteString concatenatedQuery)) === [1 .. length expectedParams]

  -- All the numbered arguments in the fragments are $1 or $2. If we re-parse the concatened query string, the first
  -- pair of Int args should be the only args. This is a strange assertion to make, but might as well
  -- make it if it's valid
  let (reparsedQuery, reparsedArgList) = case fragments of
        [] -> ("", [])
        ((_, firstFragArgs@(arg1, arg2)) : _) -> (mkQuery (mconcat $ map fst fragments) firstFragArgs, [arg1, arg2])
  map ($ encCtx) reparsedQuery.queryParams === map (\argValue -> (intEncoder.toTypeOid encCtx, intEncoder.toPgField encCtx argValue)) reparsedArgList
  length (breakQueryIntoStatements reparsedQuery) === 1
  extractPlaceholders (decodeUtf8 (queryToByteString reparsedQuery)) === [1, 2]

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

toComparableParams :: (ToPgRow a) => a -> [(Maybe Oid, BinaryField)]
toComparableParams = map ($ EncodingContext builtinPgTypesMap) . rowEncoder.toPgParams

-- | Queries built with mkQuery, including reused $N placeholders.
genMkQuery :: Gen (Query, [(Maybe Oid, BinaryField)])
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

data SomeRecord = SomeRecord {field1 :: Int, field2 :: Int}

data SomeGenericEnum = EVal1 | EVal2 | EVal3
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (ToPgField) via (LowerCasedPgEnum SomeGenericEnum)

data IntAndBool = IntAndBool {ibInt :: Int, ibBool :: Bool}
  deriving stock (Eq, Show)

instance ToPgField IntAndBool where
  fieldEncoder =
    typeFieldEncoder (typeOidWithName "int_and_bool") $
      compositeTypeEncoder $
        contramap (\(IntAndBool i b) -> (fromIntegral i :: Int32, b)) rowEncoder

-- | This exists to test TypeApplications inside quasiquoters.
polyFunc42 :: Proxy a -> Int
polyFunc42 _ = 42

infixFunc :: Char -> String -> String
infixFunc c s = c : s

-- | Queries built with the sql quasiquoter and #{} interpolation.
-- These test a variety of GHC extensions and language syntax/features
-- inside quasiquoters.
genInterpolatedQuery :: Gen (Query, [(Maybe Oid, BinaryField)])
genInterpolatedQuery =
  Gen.choice
    [ pure ([sql|SELECT 1, '#{x}', '^{y}';|], []),
      do
        x <- genInt
        y <- genInt
        c <- genChar
        pure ([sql|SELECT #{c `infixFunc` "abc"} #{if True then x else 0}, #{polyFunc42 (Proxy @String)}, #{Vector.fromList $ 37 : [45, y]};|], toComparableParams (c `infixFunc` "abc", x, polyFunc42 (Proxy @String), Vector.fromList [37, 45, y])),
      do
        x <- SomeRecord <$> genInt <*> genInt
        y <- genInt
        z <- Gen.bool
        pure ([sql|SELECT #{x.field1}, #{-(x.field2)}, #{IntAndBool { ibInt = y, {- Some comment -} ibBool = z }}, #{'a'};|], toComparableParams (x.field1, -(x.field2), IntAndBool y z, 'a')),
      do
        x <- genInt
        y <- genInt
        z <- genInt
        e :: SomeGenericEnum <- Gen.enum minBound maxBound
        pure ([sql|SELECT #{x :: Int}, #{e} FROM t WHERE #{(y :: Int) :: Int} BETWEEN 0 AND #{fromIntegral z + 1.421::Float};|], toComparableParams (x, e, y, fromIntegral z + 1.421 :: Float)),
      do
        x <- genInt
        b <- Gen.bool
        pure
          ( [sql|SELECT 42+#{let y = x + 1 in y*9}, #{fst <$> Just (b, False)}, #{case compare x 0 of
                                                      !EQ -> "abc"::Text
                                                      GT -> "cde"
                                                      LT -> "xyz"
                                                      _ -> error "Impossible"};|],
            toComparableParams
              ( let y = x + 1 in y * 9,
                fst <$> Just (b, False),
                case compare x 0 of
                  !EQ -> "abc" :: Text
                  GT -> "cde"
                  LT -> "xyz"
              )
          )
    ]

-- | Queries built with ^{} embedded queries, including reused placeholders.
genEmbeddedQuery :: Gen (Query, [(Maybe Oid, BinaryField)])
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
        pure ([sql|SELECT ^{inner1}, ^{inner2};|], toComparableParams (x, y, z)),
      do
        rows <- Gen.list (Range.linear 2 5) ((,) <$> genInt <*> genInt)
        let valuesQuery = vALUES rows
        pure ([sql|SELECT * FROM (^{valuesQuery}) AS t(a, b);|], concatMap toComparableParams rows),
      do
        x <- genInt
        y <- genInt
        pure ([sql|SELECT #{x} FROM ^{escapeIdentifier "some_table"} WHERE z = #{y};|], toComparableParams (x, y))
    ]

-- | Mix of mkQuery, #{} interpolation, and ^{} embedding.
genMixedQuery :: Gen (Query, [(Maybe Oid, BinaryField)])
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
        pure ([sql|SELECT #{x}, '#{x}', ^{inner}, (#{"abc}}}" ++ w});|], toComparableParams (x, y, z, "abc}}}" ++ w)),
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

-- | Extract all $N placeholder numbers from SQL text, in the order they first occur
-- (later repeats of an already-seen number are dropped, but the occurrence order is not
-- otherwise touched, so a placeholder numbered out of order is still detected).
-- This is a different implementation from the application, and it is so on purpose.
extractPlaceholders :: Text -> [Int]
extractPlaceholders = List.nub . go
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
