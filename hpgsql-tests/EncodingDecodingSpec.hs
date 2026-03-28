{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EncodingDecodingSpec where

import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, NominalDiffTime, UTCTime (..), fromGregorian, picosecondsToDiffTime, secondsToDiffTime)
import Data.Time.LocalTime (CalendarDiffTime (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsgAndStmt,
    withRollback,
  )
import GHC.Float (float2Double)
import GHC.Generics (Generic)
import HPgsql
import HPgsql.Encoding (AllowNull (..), ColumnInfo (..), FieldParser (..), LowerCasedPgEnum (..), ToPgField (..), anyTypeDecoder, compositeTypeParser, singleColRowParser)
import HPgsql.Query (mkQuery, sql)
import HPgsql.TypeInfo (Oid, TypeInfo (..))
import HPgsql.Types (PGArray (..), PgJson, Values (..), valuesToQuery)
import Hedgehog (PropertyT, (===))
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import TestUtils (genJsonValue)

spec :: Spec
spec = parallel $ do
  aroundConn $ describe "Encoding and decoding" $ do
    it
      "Values round-trip"
      valuesRoundTrip
    it
      "bytea values round-trip"
      byteaValuesRoundTrip
    it
      "Date types round-trip"
      dateAndTimestampTzRoundTrip
    it
      "Numeric values round-trip"
      numericValuesRoundTrip
    it
      "Numeric values round-trip even when result types are larger"
      numericValuesRoundTripEvenWhenTargetTypesAreLarger
    it
      "Numeric extreme values round-trip"
      numericExtremeValuesRoundTrip
    it
      "Json values round-trip"
      jsonValuesRoundTrip
    it
      "Date decoding"
      dateDecoding
    it
      "Date encoding"
      dateEncoding
    it
      "Timestamp decoding"
      timestampDecoding
    it
      "Timestamp encoding"
      timestampEncoding
    it
      "Less usual types"
      lessUsualTypes
  -- it
  --   "Values type round-trip"
  --   valuesTypeRoundTrip
  aroundConn $ describe "Custom types" $ do
    it "Composite type" queryCompositeType
    it
      "Querying array types"
      queryArrayTypes
    it
      "Querying enum types"
      queryEnumTypes
    it
      "Generically derived types"
      queryGenericallyDerivedTypes

valuesRoundTrip :: HPgConnection -> IO ()
valuesRoundTrip conn = do
  -- TODO: Property-based test to generate the values
  -- TODO: Include NULLs
  -- TODO: Test +-infinity for types where we can
  -- TODO: Test all types in the regions of values close to `minBound`, 0, and `maxBound`
  -- TODO: Test floats, timestamptz and other very granular but discrete type in the regions of values
  --       close to `minBound`, 0, and `maxBound`, with e.g. microsecond precision/fractional values
  -- TODO: Test +-Infinity and NaN for floats and doubles
  let row = ((-49) :: Int, False :: Bool, 2 :: Int16, 3 :: Int32, fromGregorian 1900 02 28, 42 :: Int64, UTCTime (fromGregorian 1999 12 31) 0, '意' :: Char, '&' :: Char, CalendarDiffTime 3 86403, Aeson.Null)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11" row) `shouldReturn` [row]

byteaValuesRoundTrip :: HPgConnection -> PropertyT IO ()
byteaValuesRoundTrip conn = hedgehog $ do
  someBs :: ByteString <- Gen.forAll $ Gen.bytes (Gen.linear 0 50)
  let lazyBs :: LBS.ByteString = LBS.fromStrict someBs
      row = (someBs, lazyBs)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2" row)
  res === [row]

dateAndTimestampTzRoundTrip :: HPgConnection -> PropertyT IO ()
dateAndTimestampTzRoundTrip conn = hedgehog $ do
  -- PG type value limits: https://www.postgresql.org/docs/current/datatype-datetime.html
  yearForDate :: Integer <- Gen.forAll $ Gen.integral (Gen.linear (-4712) 5874896)
  yearForTimetz :: Integer <- Gen.forAll $ Gen.integral (Gen.linear (-4712) 294275)
  month :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 12
  day :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 32 -- In case there are some weird calendar dates, go all the way to 32
  timeOfDay :: DiffTime <- Gen.forAll $ fmap (picosecondsToDiffTime . (* 1_000_000)) $ Gen.integral $ Gen.linear 0 86_400_000_000 -- Postgres has microsecond precision, so don't go beyond that
  someIntervalLessThan1Month :: NominalDiffTime <- Gen.forAll $ fmap (realToFrac . picosecondsToDiffTime . (* 1_000_000)) $ Gen.integral $ Gen.linear 0 (27 * 86_400_000_000) -- Postgres has microsecond precision, so don't go beyond that
  someNumberOfMonths :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-1000) 1000
  let date = fromGregorian yearForDate month day
      timetz = UTCTime (fromGregorian yearForTimetz month day) timeOfDay
      someInterval = CalendarDiffTime someNumberOfMonths someIntervalLessThan1Month
      row = (date, date, timetz, someInterval)
      rowRes = (date, date, UTCTime (fromGregorian 1900 1 1) 13, UTCTime (fromGregorian 1993 4 15) (11 * 3600 + 49 * 60 + 55 + 0.5), timetz, someInterval)
  -- TODO: If we change the server's timezone, does this still round-trip?
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, '1900-01-01T00:00:13Z'::timestamptz, '1993-04-15T11:49:55.5Z'::timestamptz, $3, $4" row)
  res === [rowRes]

numericValuesRoundTrip :: HPgConnection -> PropertyT IO ()
numericValuesRoundTrip conn = hedgehog $ do
  floatVal :: Float <- Gen.forAll $ Gen.float $ Gen.exponentialFloatFrom 0 (-1e38) 1e38
  floatVal2 :: Float <- Gen.forAll $ Gen.float $ Gen.linearFracFrom 0 (-1e38) 1e38
  doubleVal :: Double <- Gen.forAll $ Gen.double $ Gen.exponentialFloatFrom 0 (-1e308) 1e308
  doubleVal2 :: Double <- Gen.forAll $ Gen.double $ Gen.linearFracFrom 0 (-1e308) 1e308
  integerVal :: Integer <- Gen.forAll $ (*) <$> (fromIntegral @Int64 <$> Gen.enumBounded) <*> (fromIntegral @Int64 <$> Gen.enumBounded)
  let row = (1.521 :: Double, 1.521 :: Double, 1.521 :: Scientific, floatVal, floatVal2, doubleVal, doubleVal2, integerVal)
      rowRes = (1.521 :: Scientific, 1.5 :: Scientific, 1.521 :: Scientific, floatVal, floatVal2, doubleVal, doubleVal2, integerVal)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1::numeric, $2::numeric(4,1), $3, $4, $5, $6, $7, $8" row)
  res === [rowRes]

numericValuesRoundTripEvenWhenTargetTypesAreLarger :: HPgConnection -> PropertyT IO ()
numericValuesRoundTripEvenWhenTargetTypesAreLarger conn = hedgehog $ do
  floatVal :: Float <- Gen.forAll $ Gen.float $ Gen.exponentialFloatFrom 0 (-1e38) 1e38
  int2Val :: Int16 <- Gen.forAll Gen.enumBounded
  int4Val :: Int32 <- Gen.forAll Gen.enumBounded
  int8Val :: Int64 <- Gen.forAll Gen.enumBounded
  let row = (floatVal, int2Val, int2Val, int2Val, int2Val, int4Val, int4Val, int4Val, int8Val, int8Val)
      rowRes = (float2Double floatVal, fromIntegral int2Val :: Int32, fromIntegral int2Val :: Int64, fromIntegral int2Val :: Integer, fromIntegral int2Val :: Scientific, fromIntegral int4Val :: Int64, fromIntegral int4Val :: Integer, fromIntegral int4Val :: Scientific, fromIntegral int8Val :: Integer, fromIntegral int8Val :: Scientific)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" row)
  res === [rowRes]

numericExtremeValuesRoundTrip :: HPgConnection -> IO ()
numericExtremeValuesRoundTrip conn = do
  -- Integer boundary values
  let int16Row = (minBound :: Int16, maxBound :: Int16)
      int32Row = (minBound :: Int32, maxBound :: Int32)
      int64Row = (minBound :: Int64, maxBound :: Int64)
  queryWith rowParser conn (mkQuery "SELECT $1, $2" int16Row) `shouldReturn` [int16Row]
  queryWith rowParser conn (mkQuery "SELECT $1, $2" int32Row) `shouldReturn` [int32Row]
  queryWith rowParser conn (mkQuery "SELECT $1, $2" int64Row) `shouldReturn` [int64Row]
  -- NaN for Float and Double
  [(f :: Float, d :: Double)] <- queryWith rowParser conn (mkQuery "SELECT $1, $2" (nanFloat, nanDouble))
  f `shouldSatisfy` isNaN
  d `shouldSatisfy` isNaN
  -- +-Infinity for Float and Double
  let infRow = (posInfFloat, negInfFloat, posInfDouble, negInfDouble)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4" infRow) `shouldReturn` [infRow]
  -- NaN and +-Infinity encoded as Float, decoded as Double
  [(d1 :: Double, d2 :: Double, d3 :: Double)] <- queryWith rowParser conn (mkQuery "SELECT $1, $2, $3" (nanFloat, posInfFloat, negInfFloat))
  d1 `shouldSatisfy` isNaN
  d2 `shouldBe` posInfDouble
  d3 `shouldBe` negInfDouble
  where
    nanFloat = (0 / 0) :: Float
    nanDouble = (0 / 0) :: Double
    posInfFloat = (1 / 0) :: Float
    negInfFloat = ((-1) / 0) :: Float
    posInfDouble = (1 / 0) :: Double
    negInfDouble = ((-1) / 0) :: Double

jsonValuesRoundTrip :: HPgConnection -> PropertyT IO ()
jsonValuesRoundTrip conn = hedgehog $ do
  jsonVal1 :: Aeson.Value <- Gen.forAll genJsonValue
  jsonVal2 :: Aeson.Value <- Gen.forAll genJsonValue
  jsonVal3 :: Aeson.Value <- Gen.forAll genJsonValue
  let row = (jsonVal1, jsonVal2, jsonVal3)
  [(v1, v2, v3, v4) :: (Aeson.Value, Aeson.Value, PgJson, PgJson)] <- liftIO $ queryWith rowParser conn [sql|SELECT #{jsonVal1}, #{jsonVal1}::jsonb, #{jsonVal2}::json, #{jsonVal3}::jsonb|]
  v1 === jsonVal1
  v2 === jsonVal1
  Aeson.toJSON v3 === jsonVal2
  Aeson.toJSON v4 === jsonVal3

dateDecoding :: HPgConnection -> IO ()
dateDecoding conn = do
  let rowRes = (fromGregorian 1999 12 31, fromGregorian 2010 01 01, fromGregorian 2011 07 04, fromGregorian 1981 03 17)
  queryWith rowParser conn (mkQuery "SELECT '1999-12-31'::date, '2010-01-01'::date, '2011-07-04'::date, '1981-03-17'::date" ()) `shouldReturn` [rowRes]

dateEncoding :: HPgConnection -> IO ()
dateEncoding conn = do
  let row = (fromGregorian 1999 12 31, fromGregorian 2010 01 01, fromGregorian 2011 07 04, fromGregorian 1981 03 17)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4" row) `shouldReturn` [row]

timestampDecoding :: HPgConnection -> IO ()
timestampDecoding conn = do
  let rowRes = (UTCTime (fromGregorian 1999 12 31) 0, UTCTime (fromGregorian 2010 01 01) 0, UTCTime (fromGregorian 2011 07 04) (secondsToDiffTime 1), UTCTime (fromGregorian 1981 03 17) 43200, UTCTime (fromGregorian 1900 11 30) 43201)
  queryWith rowParser conn (mkQuery "SELECT '1999-12-31'::timestamptz, '2010-01-01'::timestamptz, '2011-07-04T00:00:01Z'::timestamptz, '1981-03-17T12:00:00Z'::timestamptz, '1900-11-30T12:00:01Z'::timestamptz" ()) `shouldReturn` [rowRes]

timestampEncoding :: HPgConnection -> IO ()
timestampEncoding conn = do
  let row = (UTCTime (fromGregorian 1999 12 31) 0, UTCTime (fromGregorian 2010 01 01) 0, UTCTime (fromGregorian 2011 07 04) (secondsToDiffTime 1), UTCTime (fromGregorian 1981 03 17) 0, UTCTime (fromGregorian 1981 03 17) 43200)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5" row) `shouldReturn` [row]

lessUsualTypes :: HPgConnection -> IO ()
lessUsualTypes conn = do
  -- Just making sure this doesn't throw is likely enough
  void $ queryWith (rowParser @(Oid, Int, String, Char)) conn "SELECT oid, typlen, typname, typcategory FROM pg_type"
  void $ queryWith (rowParser @(Oid, Int16, String, Char)) conn "SELECT oid, typlen, typname, typcategory FROM pg_type"

queryCompositeType :: HPgConnection -> IO ()
queryCompositeType conn = withRollback conn $ do
  -- TODO: Test composite types inside composite types
  -- TODO: Test empty composite type
  -- TODO: Test that composite types with few or more columns than expected generate good error messages
  -- TODO: Test that composite types with the right number of columns but wrong attribute types generates good error messages
  -- TODO: Test that one can create a record type with FromPgField and ToPgField instances!
  execute conn "CREATE TYPE int_and_bool AS (numfield INT, boolfield BOOL);"
  execute conn "CREATE TYPE mixed_bin_text AS (numfield INT, textfield TEXT);"
  let intAndBoolParser = singleColRowParser $ compositeTypeParser DisallowNull $ rowParser @(Int, Bool)
      intAndTextParser = singleColRowParser $ compositeTypeParser DisallowNull $ rowParser @(Int, Text)
      intAndBoolParserNullableFields = singleColRowParser $ compositeTypeParser DisallowNull $ rowParser @(Maybe Int, Maybe Bool)
      intAndBoolParserNullableValue = singleColRowParser $ compositeTypeParser AllowNull $ rowParser @(Int, Bool)
  queryWith intAndBoolParser conn (mkQuery "SELECT ROW($1,$2)::int_and_bool" (14 :: Int, True)) `shouldReturn` [(14, True)]
  queryWith ((,,) <$> intAndBoolParserNullableFields <*> intAndBoolParserNullableFields <*> intAndBoolParserNullableFields) conn (mkQuery "SELECT ROW(NULL,$1)::int_and_bool, ROW($2,NULL)::int_and_bool, ROW(NULL, NULL)::int_and_bool" (True, 27 :: Int)) `shouldReturn` [((Nothing, Just True), (Just 27, Nothing), (Nothing, Nothing))]
  queryWith ((,) <$> intAndBoolParser <*> intAndBoolParser) conn (mkQuery "SELECT ROW($1,$2)::int_and_bool, ROW($3,$4)::int_and_bool" ((-42) :: Int, True, 91 :: Int, False)) `shouldReturn` [((-42, True), (91, False))]
  queryWith intAndTextParser conn (mkQuery "SELECT ROW($1,$2)::mixed_bin_text" (14 :: Int, "abc" :: Text)) `shouldReturn` [(14, "abc")]
  queryWith intAndBoolParserNullableValue conn "SELECT NULL::int_and_bool" `shouldReturn` [Nothing]

queryArrayTypes :: HPgConnection -> PropertyT IO ()
queryArrayTypes conn = hedgehog $ do
  -- TODO: hedgehog gen arrays with varying lengths, NULLs, etc.
  intArrDim1 <- fmap Vector.fromList $ Gen.forAll $ Gen.list (Gen.linear 0 20) $ Gen.int (Gen.linearFrom 0 (-1000) 1000)
  liftIO $ do
    queryWith rowParser conn (mkQuery "SELECT $1, $1" (Only intArrDim1)) `shouldReturn` [(intArrDim1, intArrDim1)]
    queryWith (rowParser @(Only (Vector Int))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (13 :: Int, 31 :: Int, 45 :: Int)) `shouldReturn` [Only $ Vector.fromList [13 :: Int, 31, 45]]
    queryWith (rowParser @(Only (Vector Int16))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (13 :: Int16, 49 :: Int16, 91 :: Int16)) `shouldReturn` [Only $ Vector.fromList [13, 49, 91]]
    queryWith (rowParser @(Only (Vector (Maybe Int16)))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (13 :: Int16, Nothing :: Maybe Int16, Just (91 :: Int16))) `shouldReturn` [Only $ Vector.fromList [Just 13, Nothing, Just 91]]
    queryWith (rowParser @(Only (Vector (Maybe Text)))) conn (mkQuery "SELECT ARRAY[$1,$2,$3] -- Maybe Text" (Just ("Hello" :: Text), Nothing :: Maybe String, Just ("again" :: Text))) `shouldReturn` [Only $ Vector.fromList [Just "Hello", Nothing, Just "again"]]
    queryWith (rowParser @(Only (Vector Aeson.Value))) conn (mkQuery "SELECT ARRAY[$1,$2,$3] -- json" (Aeson.String "Hello", Aeson.Null, Aeson.Number 4)) `shouldReturn` [Only $ Vector.fromList [Aeson.String "Hello", Aeson.Null, Aeson.Number 4]]
    let multiDimArray1 = Vector.fromList [Vector.fromList [1, 2, 3, 4], Vector.fromList [4, 5, 6, 7 :: Int]]
    query conn [sql|SELECT ARRAY[ARRAY[1,2,3,4],ARRAY[4,5,6, 7]]|] `shouldReturn` [Only multiDimArray1]
    let multiDimArray2 = Vector.fromList [Vector.fromList [1, 2, 3], Vector.fromList [4, 5, 6 :: Int], Vector.fromList [7, 8, 9 :: Int]]
    query conn [sql|SELECT ARRAY[ARRAY[1,2,3],ARRAY[4,5,6], ARRAY[7,8,9]]|] `shouldReturn` [Only multiDimArray2]

data MyEnum = Val1 | Val2 | Val3
  deriving stock (Eq, Show)

instance FromPgField MyEnum where
  fieldParser =
    let convert = \case
          "val1" -> Val1
          "val2" -> Val2
          "val3" -> Val3
          _ -> error "Invalid value for MyEnum"
     in convert <$> anyTypeDecoder

myEnumFieldParserWithTypeInfoCheck :: FieldParser MyEnum
myEnumFieldParserWithTypeInfoCheck =
  let convert = \case
        "val1" -> Val1
        "val2" -> Val2
        "val3" -> Val3
        _ -> error "Invalid value for MyEnum"
      base = convert <$> anyTypeDecoder
   in base
        { allowedPgTypes = \colInfo ->
            (typeName <$> Map.lookup colInfo.typeOid colInfo.typeInfoCache) == Just "myenum"
        }

instance ToPgField MyEnum where
  toPgField tyiCache =
    toPgField tyiCache . \case
      Val1 -> "val1" :: Text
      Val2 -> "val2"
      Val3 -> "val3"

queryEnumTypes :: HPgConnection -> IO ()
queryEnumTypes conn = withRollback conn $ do
  execute conn "CREATE TYPE myenum AS ENUM ('val1', 'val2', 'val3');"
  queryWith (rowParser @(MyEnum, MyEnum, MyEnum, Maybe MyEnum)) conn "SELECT 'val1'::myenum, 'val2'::myenum, 'val3'::myenum, NULL::myenum" `shouldReturn` [(Val1, Val2, Val3, Nothing)]
  queryWith (rowParser @(MyEnum, MyEnum, MyEnum, Maybe MyEnum)) conn (mkQuery "SELECT $1, $2, $3, $4" (Val1, Val2, Val3, Nothing :: Maybe MyEnum)) `shouldReturn` [(Val1, Val2, Val3, Nothing)]
  -- The statement below will fail because the new myenum type is not in the typeCache
  -- yet. Then we add it and it will pass
  queryWith (singleColRowParser myEnumFieldParserWithTypeInfoCheck) conn "SELECT 'val2'::myenum"
    `shouldThrow` irrecoverableErrorWithMsgAndStmt "SELECT 'val2'::myenum" "Query result column types do not match expected column types"
  queryWith (singleColRowParser myEnumFieldParserWithTypeInfoCheck) conn "SELECT ARRAY['val2'::myenum]"
    `shouldThrow` irrecoverableErrorWithMsgAndStmt "SELECT ARRAY['val2'::myenum]" "Query result column types do not match expected column types"
  (refreshTyiCacheAction, queryRes) <-
    runPipeline conn $
      (,)
        <$> refreshTypeInfoCache conn
        <*> pipelineL (singleColRowParser myEnumFieldParserWithTypeInfoCheck) "SELECT 'val2'::myenum"
  refreshTyiCacheAction
  queryRes `shouldReturn` [Val2]
  query conn "SELECT ARRAY['val2'::myenum]" `shouldReturn` [Only (PGArray [Val2])]

data SomeGenericEnum = EVal1 | EVal2 | EVal3
  deriving stock (Eq, Generic, Show)
  deriving (FromPgField) via (LowerCasedPgEnum SomeGenericEnum)

data SomeGenericRecord = SomeGenericRecord
  { a :: Int,
    b :: SomeGenericEnum,
    c :: Text,
    d :: Bool,
    e :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromPgRow)

data SomeGenericProdType
  = SomeGenericProdType
      Int
      SomeGenericEnum
      Text
      Bool
      Bool
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromPgRow)

queryGenericallyDerivedTypes :: HPgConnection -> IO ()
queryGenericallyDerivedTypes conn = withRollback conn $ do
  execute conn "CREATE TYPE myenum AS ENUM ('eval1', 'eval2', 'eval3');"
  queryWith rowParser conn "SELECT 13, 'eval2'::myenum, 'Some text', true, false" `shouldReturn` [SomeGenericRecord 13 EVal2 "Some text" True False]
  queryWith rowParser conn "SELECT 13, 'eval2'::myenum, 'Some text', true, false" `shouldReturn` [SomeGenericProdType 13 EVal2 "Some text" True False]
  queryWith rowParser conn "SELECT 'eval1'::myenum, 'eval2'::myenum, 'eval3'::myenum" `shouldReturn` [(EVal1, EVal2, EVal3)]

-- valuesTypeRoundTrip :: HPgConnection -> PropertyT IO ()
-- valuesTypeRoundTrip conn = hedgehog $ do
--   rows <- Gen.forAll $ Gen.list (Gen.linear 1 20) $ (,) <$> Gen.int (Gen.linearFrom 0 (-1000) 1000) <*> Gen.bool
--   let valsQuery = valuesToQuery (Values rows)
--   results <- liftIO $ query conn [sql|WITH t AS (^{valsQuery}) SELECT * FROM t ORDER BY 1, 2|]
--   results === List.sort rows
