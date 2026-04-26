module EncodingDecodingSpec where

import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time (Day, DiffTime, LocalTime (..), NominalDiffTime, TimeOfDay, UTCTime (..), ZonedTime (..), fromGregorian, picosecondsToDiffTime, secondsToDiffTime, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (CalendarDiffTime (..))
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsgAndStmt,
    withRollback,
  )
import GHC.Float (float2Double)
import GHC.Generics (Generic)
import Hedgehog (PropertyT, annotateShow, (===))
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Gen
import Hpgsql
import Hpgsql.Encoding (AllowNull (..), ColumnInfo (..), EncodingContext (..), FieldEncoder (..), FieldParser (..), LowerCasedPgEnum (..), ToPgField (..), ToPgRow, anyTypeDecoder, compositeTypeParser, mkUntypedFieldEncoder, singleColRowParser)
import Hpgsql.Pipeline (pipeline, pipelineWith, runPipeline)
import Hpgsql.Query (mkQuery, sql, vALUES)
import Hpgsql.Time (Unbounded (..))
import Hpgsql.TypeInfo (Oid, TypeInfo (..))
import Hpgsql.Types (PGArray (..), PgJson)
import Numeric (showHex)
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
      "Rational values round-trip"
      rationalValuesRoundTrip
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
    it
      "bytea text decoding"
      byteaTextDecoding
    it
      "Date and timestamp text decoding"
      dateAndTimestampTextDecoding
    it
      "Numeric text decoding"
      numericTextDecoding
    it
      "Numeric text decoding with larger result types"
      numericTextDecodingLargerTypes
    it
      "Numeric extreme text decoding"
      numericExtremeTextDecoding
    it
      "UUID values round-trip"
      uuidRoundTrip
    it
      "UUID text decoding"
      uuidTextDecoding
    it
      "CI Text values round-trip"
      ciTextRoundTrip
    it
      "CI Text text decoding"
      ciTextTextDecoding
    it
      "LocalTime values round-trip"
      localTimeRoundTrip
    it
      "LocalTime text decoding"
      localTimeTextDecoding
    it
      "Json text decoding"
      jsonTextDecoding
    it
      "Values type round-trip"
      valuesTypeRoundTrip
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
    it
      "Generically derived types round-trip"
      queryGenericallyDerivedTypesRoundTrip

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
  let genBs = Gen.bytes (Gen.linear 0 50)
  (b1, b2, b3, b4, b5) <- Gen.forAll $ (,,,,) <$> genBs <*> genBs <*> genBs <*> genBs <*> genBs
  let row = (b1, LBS.fromStrict b1, b2, LBS.fromStrict b2, b3, LBS.fromStrict b3, b4, LBS.fromStrict b4, b5, LBS.fromStrict b5)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" row)
  res === [row]

dateAndTimestampTzRoundTrip :: HPgConnection -> PropertyT IO ()
dateAndTimestampTzRoundTrip conn = hedgehog $ do
  -- PG type value limits: https://www.postgresql.org/docs/current/datatype-datetime.html
  yearForDate :: Integer <- Gen.forAll $ Gen.integral (Gen.linear (-4712) 5874896)
  yearForTimetz :: Integer <- Gen.forAll $ Gen.integral (Gen.linear (-4712) 294275)
  month :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 12
  day :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 32 -- In case there are some weird calendar dates, go all the way to 32
  timeOfDay :: DiffTime <- Gen.forAll $ fmap (picosecondsToDiffTime . (* 1_000_000)) $ Gen.integral $ Gen.linear 0 86_400_000_000 -- Postgres has microsecond precision, so don't go beyond that
  someIntervalTimeMicros :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-100 * 86_400_000_000) (100 * 86_400_000_000) -- +-100 days in microseconds
  someNumberOfMonths :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-1000) 1000
  someNominalDiffTimeMicros :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-1_000_000_000_000) 1_000_000_000_000
  let date = fromGregorian yearForDate month day
      timetz = UTCTime (fromGregorian yearForTimetz month day) timeOfDay
      someIntervalTime :: NominalDiffTime = realToFrac $ picosecondsToDiffTime (someIntervalTimeMicros * 1_000_000)
      someCalendarDiffTime = CalendarDiffTime someNumberOfMonths someIntervalTime
      someNominalDiffTime :: NominalDiffTime = realToFrac $ picosecondsToDiffTime (someNominalDiffTimeMicros * 1_000_000)
      row = (date, date, timetz, someCalendarDiffTime, Finite timetz, Finite date, someNominalDiffTime)
      rowRes = (date, date, UTCTime (fromGregorian 1900 1 1) 13, UTCTime (fromGregorian 1993 4 15) (11 * 3600 + 49 * 60 + 55 + 0.5), timetz, someCalendarDiffTime, Finite timetz, Finite date, CalendarDiffTime 0 0)
  -- TODO: If we change the server's timezone, does this still round-trip?
  annotateShow someNominalDiffTime
  let (nomSecs, nomRemMicros) = someNominalDiffTimeMicros `quotRem` 1_000_000
  res <- liftIO $ queryWith rowParser conn (mkQuery ("SELECT $1, $2, '1900-01-01T00:00:13Z'::timestamptz, '1993-04-15T11:49:55.5Z'::timestamptz, $3, $4, $5, $6, $7 - INTERVAL '" <> fromString (show nomSecs) <> " seconds " <> fromString (show nomRemMicros) <> " microseconds'") row)
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

rationalValuesRoundTrip :: HPgConnection -> IO ()
rationalValuesRoundTrip conn = do
  -- Rationals with terminating decimal representations round-trip exactly
  let row = (1 % 2 :: Rational, 3 % 4 :: Rational, 7 % 8 :: Rational, 1 % 5 :: Rational, (-3) % 20 :: Rational, 0 % 1 :: Rational, 123456789 % 1000 :: Rational)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7" row) `shouldReturn` [row]
  -- Decoding from integer types
  let intRow = (42 :: Int32, (-7) :: Int16)
      ratRes = (42 % 1 :: Rational, (-7) % 1 :: Rational)
  queryWith rowParser conn (mkQuery "SELECT $1, $2" intRow) `shouldReturn` [ratRes]

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
  let rowRes = (fromGregorian 1999 12 31, fromGregorian 2010 01 01, fromGregorian 2011 07 04, fromGregorian 1981 03 17, NegInfinity @UTCTime, PosInfinity @UTCTime, NegInfinity @Day, PosInfinity @Day)
  queryWith rowParser conn (mkQuery "SELECT '1999-12-31'::date, '2010-01-01'::date, '2011-07-04'::date, '1981-03-17'::date, '-infinity'::timestamptz, 'infinity'::timestamptz, '-infinity'::date, 'infinity'::date" ()) `shouldReturn` [rowRes]

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
  query1 conn (mkQuery "SELECT $1, $2, $3, $4, $5" row) `shouldReturn` row

lessUsualTypes :: HPgConnection -> IO ()
lessUsualTypes conn = do
  -- Just making sure this doesn't throw is likely enough
  void $ queryWith (rowParser @(Oid, Int, String, Char)) conn "SELECT oid, typlen, typname, typcategory FROM pg_type"
  void $ queryWith (rowParser @(Oid, Int16, String, Char)) conn "SELECT oid, typlen, typname, typcategory FROM pg_type"

byteaTextDecoding :: HPgConnection -> PropertyT IO ()
byteaTextDecoding conn = hedgehog $ do
  someBs :: ByteString <- Gen.forAll $ Gen.bytes (Gen.linear 0 50)
  let lazyBs :: LBS.ByteString = LBS.fromStrict someBs
      hexStr = concatMap (\w -> let s = showHex w "" in if length s < 2 then '0' : s else s) (BS.unpack someBs)
  res <- liftIO $ queryMay conn (fromString $ "SELECT '\\x" <> hexStr <> "'::bytea, '\\x" <> hexStr <> "'::bytea")
  res === Just (someBs, lazyBs)

dateAndTimestampTextDecoding :: HPgConnection -> PropertyT IO ()
dateAndTimestampTextDecoding conn = hedgehog $ do
  yearForDate :: Integer <- Gen.forAll $ Gen.integral (Gen.linear 1 9999)
  yearForTimetz :: Integer <- Gen.forAll $ Gen.integral (Gen.linear 1 9999)
  month :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 12
  day :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 28
  timeOfDayMicros :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear 0 86_399_999_999
  someNumberOfMonths :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-1000) 1000
  someIntervalTimeMicros :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-100 * 86_400_000_000) (100 * 86_400_000_000)
  someNominalDiffTimeMicros :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-1_000_000_000_000) 1_000_000_000_000
  let date = fromGregorian yearForDate month day
      timeOfDay = picosecondsToDiffTime (timeOfDayMicros * 1_000_000)
      timetz = UTCTime (fromGregorian yearForTimetz month day) timeOfDay
      someIntervalTime :: NominalDiffTime = realToFrac $ picosecondsToDiffTime (someIntervalTimeMicros * 1_000_000)
      someCalendarDiffTime = CalendarDiffTime someNumberOfMonths someIntervalTime
      someNominalDiffTime :: NominalDiffTime = realToFrac $ picosecondsToDiffTime (someNominalDiffTimeMicros * 1_000_000)
      (intervalSecs, intervalRemMicros) = someIntervalTimeMicros `quotRem` 1_000_000
      (nomSecs, nomRemMicros) = someNominalDiffTimeMicros `quotRem` 1_000_000
  res <-
    liftIO $
      queryWith
        rowParser
        conn
        ( fromString $
            "SELECT '"
              <> iso8601Show date
              <> "'::date"
              <> ", '"
              <> iso8601Show timetz
              <> "'::timestamptz"
              <> ", '"
              <> show someNumberOfMonths
              <> " months "
              <> show intervalSecs
              <> " seconds "
              <> show intervalRemMicros
              <> " microseconds'::interval"
              <> ", '"
              <> iso8601Show timetz
              <> "'::timestamptz"
              <> ", '"
              <> iso8601Show date
              <> "'::date"
              <> ", '"
              <> show nomSecs
              <> " seconds "
              <> show nomRemMicros
              <> " microseconds'::interval"
        )
  res === [(date, timetz, someCalendarDiffTime, Finite timetz, Finite date, CalendarDiffTime 0 someNominalDiffTime)]

numericTextDecoding :: HPgConnection -> PropertyT IO ()
numericTextDecoding conn = hedgehog $ do
  floatVal :: Float <- Gen.forAll $ Gen.float $ Gen.exponentialFloatFrom 0 (-1e38) 1e38
  floatVal2 :: Float <- Gen.forAll $ Gen.float $ Gen.linearFracFrom 0 (-1e38) 1e38
  doubleVal :: Double <- Gen.forAll $ Gen.double $ Gen.exponentialFloatFrom 0 (-1e308) 1e308
  doubleVal2 :: Double <- Gen.forAll $ Gen.double $ Gen.linearFracFrom 0 (-1e308) 1e308
  integerVal :: Integer <- Gen.forAll $ (*) <$> (fromIntegral @Int64 <$> Gen.enumBounded) <*> (fromIntegral @Int64 <$> Gen.enumBounded)
  res <-
    liftIO $
      queryWith
        rowParser
        conn
        ( fromString $
            "SELECT '1.521'::numeric, '1.521'::numeric(4,1), '1.521'::numeric"
              <> ", '"
              <> show floatVal
              <> "'::float4"
              <> ", '"
              <> show floatVal2
              <> "'::float4"
              <> ", '"
              <> show doubleVal
              <> "'::float8"
              <> ", '"
              <> show doubleVal2
              <> "'::float8"
              <> ", '"
              <> show integerVal
              <> "'::numeric"
        )
  res === [(1.521 :: Scientific, 1.5 :: Scientific, 1.521 :: Scientific, floatVal, floatVal2, doubleVal, doubleVal2, integerVal)]

numericTextDecodingLargerTypes :: HPgConnection -> PropertyT IO ()
numericTextDecodingLargerTypes conn = hedgehog $ do
  floatVal :: Float <- Gen.forAll $ Gen.float $ Gen.exponentialFloatFrom 0 (-1e38) 1e38
  int2Val :: Int16 <- Gen.forAll Gen.enumBounded
  int4Val :: Int32 <- Gen.forAll Gen.enumBounded
  int8Val :: Int64 <- Gen.forAll Gen.enumBounded
  res <-
    liftIO $
      queryWith
        rowParser
        conn
        ( fromString $
            "SELECT '"
              <> show floatVal
              <> "'::float4"
              <> ", '"
              <> show int2Val
              <> "'::int2"
              <> ", '"
              <> show int2Val
              <> "'::int2"
              <> ", '"
              <> show int2Val
              <> "'::int2"
              <> ", '"
              <> show int2Val
              <> "'::int2"
              <> ", '"
              <> show int4Val
              <> "'::int4"
              <> ", '"
              <> show int4Val
              <> "'::int4"
              <> ", '"
              <> show int4Val
              <> "'::int4"
              <> ", '"
              <> show int8Val
              <> "'::int8"
              <> ", '"
              <> show int8Val
              <> "'::int8"
        )
  let rowRes = (float2Double floatVal, fromIntegral int2Val :: Int32, fromIntegral int2Val :: Int64, fromIntegral int2Val :: Integer, fromIntegral int2Val :: Scientific, fromIntegral int4Val :: Int64, fromIntegral int4Val :: Integer, fromIntegral int4Val :: Scientific, fromIntegral int8Val :: Integer, fromIntegral int8Val :: Scientific)
  res === [rowRes]

numericExtremeTextDecoding :: HPgConnection -> IO ()
numericExtremeTextDecoding conn = do
  queryWith rowParser conn (fromString $ "SELECT '" <> show (minBound :: Int16) <> "'::int2, '" <> show (maxBound :: Int16) <> "'::int2")
    `shouldReturn` [(minBound :: Int16, maxBound :: Int16)]
  queryWith rowParser conn (fromString $ "SELECT '" <> show (minBound :: Int32) <> "'::int4, '" <> show (maxBound :: Int32) <> "'::int4")
    `shouldReturn` [(minBound :: Int32, maxBound :: Int32)]
  queryWith rowParser conn (fromString $ "SELECT '" <> show (minBound :: Int64) <> "'::int8, '" <> show (maxBound :: Int64) <> "'::int8")
    `shouldReturn` [(minBound :: Int64, maxBound :: Int64)]
  [(f :: Float, d :: Double)] <- queryWith rowParser conn "SELECT 'NaN'::float4, 'NaN'::float8"
  f `shouldSatisfy` isNaN
  d `shouldSatisfy` isNaN
  queryWith rowParser conn "SELECT 'Infinity'::float4, '-Infinity'::float4, 'Infinity'::float8, '-Infinity'::float8"
    `shouldReturn` [((1 / 0) :: Float, ((-1) / 0) :: Float, (1 / 0) :: Double, ((-1) / 0) :: Double)]
  [(d1 :: Double, d2 :: Double, d3 :: Double)] <- queryWith rowParser conn "SELECT 'NaN'::float4, 'Infinity'::float4, '-Infinity'::float4"
  d1 `shouldSatisfy` isNaN
  d2 `shouldBe` (1 / 0 :: Double)
  d3 `shouldBe` ((-1) / 0 :: Double)

jsonTextDecoding :: HPgConnection -> PropertyT IO ()
jsonTextDecoding conn = hedgehog $ do
  jsonVal1 :: Aeson.Value <- Gen.forAll genJsonValue
  jsonVal2 :: Aeson.Value <- Gen.forAll genJsonValue
  jsonVal3 :: Aeson.Value <- Gen.forAll genJsonValue
  let encodeJson = pgEscape . Text.unpack . TE.decodeUtf8 . LBS.toStrict . Aeson.encode
  [(v1, v2, v3, v4) :: (Aeson.Value, Aeson.Value, PgJson, PgJson)] <-
    liftIO $
      queryWith
        rowParser
        conn
        ( fromString $
            "SELECT '"
              <> encodeJson jsonVal1
              <> "'::json"
              <> ", '"
              <> encodeJson jsonVal1
              <> "'::jsonb"
              <> ", '"
              <> encodeJson jsonVal2
              <> "'::json"
              <> ", '"
              <> encodeJson jsonVal3
              <> "'::jsonb"
        )
  v1 === jsonVal1
  v2 === jsonVal1
  Aeson.toJSON v3 === jsonVal2
  Aeson.toJSON v4 === jsonVal3
  where
    pgEscape = concatMap $ \case
      '\'' -> "''"
      c -> [c]

uuidRoundTrip :: HPgConnection -> PropertyT IO ()
uuidRoundTrip conn = hedgehog $ do
  let genUuid = do
        uuidBytes <- Gen.bytes (Gen.singleton 16)
        let Just uuid = UUID.fromByteString (LBS.fromStrict uuidBytes)
        pure uuid
  row <- Gen.forAll $ (,,,,,,,,,) <$> genUuid <*> genUuid <*> genUuid <*> genUuid <*> genUuid <*> genUuid <*> genUuid <*> genUuid <*> genUuid <*> genUuid
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" row)
  res === [row]

uuidTextDecoding :: HPgConnection -> PropertyT IO ()
uuidTextDecoding conn = hedgehog $ do
  uuidBytes <- Gen.forAll $ Gen.bytes (Gen.singleton 16)
  let Just uuid = UUID.fromByteString (LBS.fromStrict uuidBytes)
  res <-
    liftIO $
      queryWith
        rowParser
        conn
        (fromString $ "SELECT '" <> UUID.toString uuid <> "'::uuid")
  res === [Only uuid]

ciTextRoundTrip :: HPgConnection -> PropertyT IO ()
ciTextRoundTrip conn = hedgehog $ do
  let genCIText = CI.mk <$> Gen.text (Gen.linear 0 50) (Gen.filter (/= '\0') Gen.unicode)
      genCILazyText = CI.mk . LT.fromStrict <$> Gen.text (Gen.linear 0 50) (Gen.filter (/= '\0') Gen.unicode)
      genCIString = CI.mk <$> Gen.string (Gen.linear 0 50) (Gen.filter (/= '\0') Gen.unicode)
  row <-
    Gen.forAll $
      (,,,,,,,,,)
        <$> genCIText
        <*> genCIText
        <*> genCIText
        <*> genCIText
        <*> genCILazyText
        <*> genCILazyText
        <*> genCILazyText
        <*> genCIString
        <*> genCIString
        <*> genCIString
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" row)
  res === [row]

ciTextTextDecoding :: HPgConnection -> PropertyT IO ()
ciTextTextDecoding conn = hedgehog $ do
  someText :: Text <- Gen.forAll $ Gen.text (Gen.linear 0 50) (Gen.filter (\c -> c /= '\0' && c /= '\'') Gen.unicode)
  res <-
    liftIO $
      queryWith
        rowParser
        conn
        (fromString $ "SELECT '" <> Text.unpack someText <> "'::text, '" <> Text.unpack someText <> "'::text, '" <> Text.unpack someText <> "'::text")
  res === [(CI.mk someText, CI.mk (LT.fromStrict someText), CI.mk (Text.unpack someText))]

localTimeRoundTrip :: HPgConnection -> PropertyT IO ()
localTimeRoundTrip conn = hedgehog $ do
  let genLocalTime = do
        year <- Gen.integral (Gen.linear (-4712) 294275)
        month <- Gen.int $ Gen.linear 1 12
        day <- Gen.int $ Gen.linear 1 28
        timeOfDayMicros <- Gen.integral $ Gen.linear 0 86_399_999_999
        let localDay = fromGregorian year month day
            localTimeOfDay = timeToTimeOfDay $ picosecondsToDiffTime (timeOfDayMicros * 1_000_000)
        pure $ LocalTime localDay localTimeOfDay
  row <- Gen.forAll $ (,,,,,,,,,) <$> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" row)
  res === [row]

localTimeTextDecoding :: HPgConnection -> PropertyT IO ()
localTimeTextDecoding conn = hedgehog $ do
  let genLocalTime = do
        year <- Gen.integral (Gen.linear 1 9999)
        month <- Gen.int $ Gen.linear 1 12
        day <- Gen.int $ Gen.linear 1 28
        timeOfDayMicros <- Gen.integral $ Gen.linear 0 86_399_999_999
        let localDay = fromGregorian year month day
            localTimeOfDay = timeToTimeOfDay $ picosecondsToDiffTime (timeOfDayMicros * 1_000_000)
        pure $ LocalTime localDay localTimeOfDay
  row <- Gen.forAll $ (,,,,,,,,,) <$> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime <*> genLocalTime
  let (lt1, lt2, lt3, lt4, lt5, lt6, lt7, lt8, lt9, lt10) = row
  res <-
    liftIO $ withRollback conn $ do
      -- Doesn't seem like the timezone matters, but we set to
      -- UTC because this is a textual representation, and the
      -- test's intention is only to make sure what we don't have
      -- FromPgField and ToPgField instances for `LocalTime` that
      -- are the inverse of each other but produce bogus values
      -- nonetheless.
      execute conn "SET LOCAL timezone = 'UTC'"
      queryWith
        rowParser
        conn
        ( fromString $
            "SELECT '"
              <> iso8601Show lt1
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt2
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt3
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt4
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt5
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt6
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt7
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt8
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt9
              <> "'::timestamp"
              <> ", '"
              <> iso8601Show lt10
              <> "'::timestamp"
        )
  res === [row]

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
  nullIntArrDim1 <- fmap PGArray $ Gen.forAll $ Gen.list (Gen.linear 0 20) $ Gen.maybe $ Gen.int (Gen.linearFrom 0 (-1000) 1000)
  liftIO $ do
    queryWith rowParser conn (mkQuery "SELECT $1, $1, $2" (intArrDim1, nullIntArrDim1)) `shouldReturn` [(intArrDim1, intArrDim1, nullIntArrDim1)]
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
            (typeName <$> Map.lookup colInfo.typeOid colInfo.encodingContext.typeInfoCache) == Just "myenum"
        }

instance ToPgField MyEnum where
  fieldEncoder = mkUntypedFieldEncoder $ \encCtx ->
    (fieldEncoder @Text).toPgField encCtx . \case
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
        <*> pipelineWith (singleColRowParser myEnumFieldParserWithTypeInfoCheck) "SELECT 'val2'::myenum"
  refreshTyiCacheAction
  queryRes `shouldReturn` [Val2]
  query conn "SELECT ARRAY['val2'::myenum]" `shouldReturn` [Only (PGArray [Val2])]

data SomeGenericEnum = EVal1 | EVal2 | EVal3
  deriving stock (Eq, Generic, Show)
  deriving (FromPgField, ToPgField) via (LowerCasedPgEnum SomeGenericEnum)

data SomeGenericRecord = SomeGenericRecord
  { a :: Int,
    b :: SomeGenericEnum,
    c :: Text,
    d :: Bool,
    e :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromPgRow, ToPgRow)

data SomeGenericProdType
  = SomeGenericProdType
      Int
      SomeGenericEnum
      Text
      Bool
      Bool
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromPgRow, ToPgRow)

genSomeGenericEnum :: Gen.Gen SomeGenericEnum
genSomeGenericEnum = Gen.element [EVal1, EVal2, EVal3]

genSomeGenericRecord :: Gen.Gen SomeGenericRecord
genSomeGenericRecord =
  SomeGenericRecord
    <$> Gen.int Gen.linearBounded
    <*> genSomeGenericEnum
    <*> Gen.text (Gen.linear 0 50) (Gen.filter (/= '\0') Gen.unicode)
    <*> Gen.bool
    <*> Gen.bool

genSomeGenericProdType :: Gen.Gen SomeGenericProdType
genSomeGenericProdType =
  SomeGenericProdType
    <$> Gen.int Gen.linearBounded
    <*> genSomeGenericEnum
    <*> Gen.text (Gen.linear 0 50) (Gen.filter (/= '\0') Gen.unicode)
    <*> Gen.bool
    <*> Gen.bool

queryGenericallyDerivedTypes :: HPgConnection -> IO ()
queryGenericallyDerivedTypes conn = withRollback conn $ do
  execute conn "CREATE TYPE myenum AS ENUM ('eval1', 'eval2', 'eval3');"
  queryWith rowParser conn "SELECT 13, 'eval2'::myenum, 'Some text', true, false" `shouldReturn` [SomeGenericRecord 13 EVal2 "Some text" True False]
  queryWith rowParser conn "SELECT 13, 'eval2'::myenum, 'Some text', true, false" `shouldReturn` [SomeGenericProdType 13 EVal2 "Some text" True False]
  queryWith rowParser conn "SELECT 'eval1'::myenum, 'eval2'::myenum, 'eval3'::myenum" `shouldReturn` [(EVal1, EVal2, EVal3)]

queryGenericallyDerivedTypesRoundTrip :: HPgConnection -> PropertyT IO ()
queryGenericallyDerivedTypesRoundTrip conn = hedgehog $ do
  someRec <- Gen.forAll genSomeGenericRecord
  somePT <- Gen.forAll genSomeGenericProdType
  -- \$2 is cast to `myenum` explicitly because `LowerCasedPgEnum`'s `ToPgField`
  -- instance returns no type OID, so postgres needs the cast to pick the enum type.
  (res1, res2) <- liftIO $ withRollback conn $ do
    execute conn "CREATE TYPE myenum AS ENUM ('eval1', 'eval2', 'eval3');"
    r1 <- queryWith rowParser conn (mkQuery "SELECT $1, $2::myenum, $3, $4, $5" someRec)
    r2 <- queryWith rowParser conn (mkQuery "SELECT $1, $2::myenum, $3, $4, $5" somePT)
    pure (r1, r2)
  res1 === [someRec]
  res2 === [somePT]

valuesTypeRoundTrip :: HPgConnection -> PropertyT IO ()
valuesTypeRoundTrip conn = hedgehog $ do
  rows2Tuple <- Gen.forAll $ Gen.list (Gen.linear 0 20) $ (,) <$> Gen.int (Gen.linearFrom 0 (-1000) 1000) <*> Gen.bool
  rowsSingleVal <- Gen.forAll $ Gen.list (Gen.linear 0 20) $ Only <$> Gen.int (Gen.linearFrom 0 (-1000) 1000)
  results2Tuple <- liftIO $ query conn [sql|WITH t AS (^{vALUES rows2Tuple}) SELECT * FROM t|]
  resultsSingleVal <- liftIO $ query conn [sql|WITH t AS (^{vALUES rowsSingleVal}) SELECT * FROM t|]
  List.sort results2Tuple === List.sort rows2Tuple
  List.sort resultsSingleVal === List.sort rowsSingleVal
