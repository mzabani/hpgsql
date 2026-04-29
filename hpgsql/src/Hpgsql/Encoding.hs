{-# LANGUAGE UndecidableInstances #-}

module Hpgsql.Encoding
  ( FromPgRow (..),
    FromPgField (..),
    ToPgField (..),
    FieldEncoder (..),
    ToPgRow (..),
    RowEncoder (..),
    Only (..),
    ColumnInfo (..),
    EncodingContext (..),
    FieldDecoder (..), -- TODO: Can we export ctor?
    RowDecoder (..), -- TODO: Can we export ctor?
    LowerCasedPgEnum (..),
    (:.) (..),
    untypedFieldEncoder,
    typeFieldEncoder,
    typeOidWithName,
    typeFieldDecoder,
    typeMustBeNamed,
    rawBytesFieldDecoder,
    singleField,
    arrayField,
    toPgVectorField,
    genericFromPgRow,
    genericToPgRow,
    nullableField,
    compositeTypeParser,
  )
where

import Control.Monad (replicateM, unless, when)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Fixed (divMod')
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int16, Int32, Int64)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio)
import Data.Scientific (Scientific (..), floatingOrInteger, scientific)
import qualified Data.Serialize as Cereal
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Time (CalendarDiffDays (..), CalendarDiffTime (..), Day, LocalTime (..), NominalDiffTime, UTCTime (..), ZonedTime, diffDays, diffTimeToPicoseconds, fromGregorian, picosecondsToDiffTime, secondsToNominalDiffTime, timeOfDayToTime, timeToTimeOfDay, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Calendar.Julian (addJulianDurationClip, fromJulian)
import Data.Tuple.Only (Only (..))
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble, expt, float2Double)
import GHC.Generics (C, D, Generic (..), K1 (..), M1 (..), Meta (MetaCons), U1 (..), (:*:) (..), (:+:) (..))
import GHC.TypeLits (KnownSymbol, TypeError, symbolVal)
import qualified GHC.TypeLits as TypeLits
import Hpgsql.Builder (BinaryField (..))
import qualified Hpgsql.Builder as Builder
import qualified Hpgsql.SimpleParser as Parser
import Hpgsql.Time (Unbounded (..))
import Hpgsql.TypeInfo (EncodingContext (..), Oid (..), TypeInfo (..), boolOid, byteaOid, charOid, dateOid, float4Oid, float8Oid, int2Oid, int4Oid, int8Oid, intervalOid, jsonOid, jsonbOid, lookupTypeByName, lookupTypeByOid, nameOid, numericOid, oidOid, textOid, timestampOid, timestamptzOid, uuidOid, varcharOid, voidOid)

data ColumnInfo = ColumnInfo
  { fieldTypeOid :: !Oid,
    -- | The EncodingContext as of the moment the query ran.
    encodingContext :: !EncodingContext
  }

data FieldDecoder a = FieldDecoder
  { fieldValueDecoder :: ColumnInfo -> Maybe ByteString -> Either String a,
    allowedPgTypes :: ColumnInfo -> Bool
  }
  deriving stock (Functor)

instance Semigroup (FieldDecoder a) where
  dec1 <> dec2 =
    FieldDecoder
      { fieldValueDecoder = \cInfo ->
          let f1 = dec1.fieldValueDecoder cInfo
              f2 = dec2.fieldValueDecoder cInfo
           in \mbs ->
                let cand1 = if dec1.allowedPgTypes cInfo then f1 mbs else Left "Not first parser"
                    cand2 = if dec2.allowedPgTypes cInfo then f2 mbs else Left "Not second parser"
                 in cand1 <> cand2,
        allowedPgTypes = \cInfo -> dec1.allowedPgTypes cInfo || dec2.allowedPgTypes cInfo
      }

data RowDecoder a = RowDecoder
  { fullRowDecoder :: [ColumnInfo] -> Parser.Parser a,
    -- | Returns the same colInfos with a boolean indicating if
    -- the expected types match for each colInfo.
    rowColumnsTypeCheck :: [ColumnInfo] -> [(ColumnInfo, Bool)],
    numExpectedColumns :: !Int
  }
  deriving stock (Functor, Generic)

instance Applicative RowDecoder where
  pure v = RowDecoder (const $ pure v) (map (,True)) 0
  RowDecoder p1 tc1 nc1 <*> RowDecoder p2 tc2 nc2 = RowDecoder (\colTypes -> let (cols1, cols2) = List.splitAt nc1 colTypes in p1 cols1 <*> p2 cols2) (\colTypes -> let (cols1, cols2) = List.splitAt nc1 colTypes in tc1 cols1 ++ tc2 cols2) (nc1 + nc2)

instance (TypeError (TypeLits.Text "RowDecoder does not have a Monad instance in Hpgsql because Hpgsql type-checks the result types of queries before having access to even the first data row. Use the Applicative class to write your instances or use the Monadic decoding variants.")) => Monad RowDecoder where
  (>>=) = error "inaccessible bind in Monad RowDecoder instance"

singleField :: FieldDecoder a -> RowDecoder a
singleField (FieldDecoder {..}) =
  RowDecoder
    { fullRowDecoder = \case
        [singleColInfo] ->
          let decode = fieldValueDecoder singleColInfo
           in do
                lenNextCol <- fromIntegral <$> int32Parser
                nextColBs <-
                  if lenNextCol >= 0
                    then
                      Just <$> Parser.take lenNextCol
                    else pure Nothing
                case decode nextColBs of
                  Right v -> pure v
                  Left err -> fail err
        _ -> error "singleField expected a single column OID but got 0 or >1",
      rowColumnsTypeCheck = \case
        [singleColInfo] -> [(singleColInfo, allowedPgTypes singleColInfo)]
        _ -> error "singleField's rowColumnsTypeCheck expected a single column OID but got 0 or >1",
      numExpectedColumns = 1
    }

int32Parser :: Parser.Parser Int32
int32Parser = either fail pure . Cereal.decode @Int32 =<< Parser.take 4

class FromPgField a where
  fieldDecoder :: FieldDecoder a

class FromPgRow a where
  rowDecoder :: RowDecoder a
  default rowDecoder :: (Generic a, ProductTypeDecoder (Rep a)) => RowDecoder a
  rowDecoder = genericFromPgRow

-- | Allows you to create a @FieldDecoder@ for composite types.
-- For a type such as:
--
-- > CREATE TYPE int_and_bool AS (numfield INT, boolfield BOOL);
--
-- You can define a Haskell type as such:
--
-- > data IntAndBool = IntAndBool Int Bool
-- >
-- > instance FromPgField IntAndBool where
-- >   fieldDecoder = compositeTypeParser (rowDecoder @(Int, Bool)) <&> \(i, b) -> IntAndBool i b
compositeTypeParser :: forall a. RowDecoder a -> FieldDecoder a
compositeTypeParser (RowDecoder {..}) =
  FieldDecoder
    { fieldValueDecoder = \compositeTypeOid -> \case
        Nothing -> Left "Got NULL in composite type but it was not allowed"
        Just bs -> case Parser.parseOnly (parserForRecord compositeTypeOid.encodingContext <* Parser.endOfInput) bs of
          Parser.ParseOk v -> Right v
          Parser.ParseFail err -> Left err,
      allowedPgTypes = const True -- There's no way to enforce a custom type's OID. We only check if it's structurally the same in the parser (same subtypes in same order)
    }
  where
    parserForRecord :: EncodingContext -> Parser.Parser a
    parserForRecord encodingContext = do
      -- From https://github.com/postgres/postgres/blob/50ba65e73325cf55fedb3e1f14673d816726923b/src/backend/utils/adt/rowtypes.c#L687
      -- we can see a composite type's binary representation consists of: number of columns (Int32) + for_each_column { OID (Int32) + size_or_minus_1 (Int32) + Bytes }
      numCols <- fromIntegral <$> int32Parser
      unless (numCols == numExpectedColumns) $ fail $ "Composite type has " ++ show numCols ++ " attributes but parser expected " ++ show numExpectedColumns
      let mkColInfo oid = ColumnInfo oid encodingContext
      cols <- replicateM numCols $ do
        !oid <- Oid . fromIntegral <$> int32Parser
        (sizeBs, !size) <- Parser.match $ fromIntegral <$> int32Parser
        !bs <- Parser.take (max 0 size)
        pure (oid, sizeBs <> bs)
      let typecheckedCols = rowColumnsTypeCheck (map (mkColInfo . fst) cols)
      unless (all snd typecheckedCols) $ fail $ "Parser for composite found type OIDs " ++ show (map fst cols) ++ " but expected different"
      case Parser.parseOnly (fullRowDecoder (map (mkColInfo . fst) cols) <* Parser.endOfInput) (mconcat $ map snd cols) of
        Parser.ParseOk v -> pure v
        Parser.ParseFail err -> error $ "Error decoding composite type: " ++ show err

instance (FromPgField a) => FromPgRow (Only a) where
  rowDecoder = Only <$> singleField fieldDecoder

instance (FromPgField a, FromPgField b) => FromPgRow (a, b) where
  rowDecoder = (,) <$> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c) => FromPgRow (a, b, c) where
  rowDecoder = (,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d) => FromPgRow (a, b, c, d) where
  rowDecoder = (,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e) => FromPgRow (a, b, c, d, e) where
  rowDecoder = (,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f) => FromPgRow (a, b, c, d, e, f) where
  rowDecoder = (,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g) => FromPgRow (a, b, c, d, e, f, g) where
  rowDecoder = (,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h) => FromPgRow (a, b, c, d, e, f, g, h) where
  rowDecoder = (,,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i) => FromPgRow (a, b, c, d, e, f, g, h, i) where
  rowDecoder = (,,,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j) => FromPgRow (a, b, c, d, e, f, g, h, i, j) where
  rowDecoder = (,,,,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j, FromPgField k) => FromPgRow (a, b, c, d, e, f, g, h, i, j, k) where
  rowDecoder = (,,,,,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j, FromPgField k, FromPgField l) => FromPgRow (a, b, c, d, e, f, g, h, i, j, k, l) where
  rowDecoder = (,,,,,,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j, FromPgField k, FromPgField l, FromPgField m) => FromPgRow (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  rowDecoder = (,,,,,,,,,,,,) <$> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder <*> singleField fieldDecoder

data FieldEncoder a = FieldEncoder
  { toTypeOid :: !(EncodingContext -> Maybe Oid),
    toPgField :: !(EncodingContext -> a -> BinaryField)
  }

instance Contravariant FieldEncoder where
  contramap f fEnc = FieldEncoder {toTypeOid = fEnc.toTypeOid, toPgField = \encCtx -> let toF = fEnc.toPgField encCtx in \v -> toF (f v)}

class ToPgField a where
  fieldEncoder :: FieldEncoder a

-- | Returns a `FieldEncoder` that is sent without a type OID in queries.
-- This means postgres will try to infer the type of these arguments.
-- Check 'typedFieldEncoder' if you're interested in encoding your custom types,
-- you probably don't need this.
untypedFieldEncoder :: (EncodingContext -> a -> BinaryField) -> FieldEncoder a
untypedFieldEncoder enc = FieldEncoder {toTypeOid = \_ -> Nothing, toPgField = enc}

-- | Allows you to specify a type for a FieldEncoder. This can be useful to avoid
-- letting postgres infer types itself, which can cause errors. For example:
--
-- > data MyEnum = Val1 | Val2 | Val3
-- > myEnumFieldDecoderWithTypeInfoCheck :: FieldEncoder MyEnum
-- > myEnumFieldDecoderWithTypeInfoCheck =
-- >   let convert = \case
-- >         Val1 -> "val1" :: Text
-- >         Val2 -> "val2"
-- >         Val3 -> "val3"
-- >    in typeFieldEncoder
-- >         (typeOidWithName "my_enum")
-- >         $ contramap convert fieldEncoder
--
-- This will work unless you use non-default flags in your connection options.
typeFieldEncoder :: (EncodingContext -> Maybe Oid) -> FieldEncoder a -> FieldEncoder a
typeFieldEncoder ttoid enc = enc {toTypeOid = ttoid}

typeOidWithName :: Text -> (EncodingContext -> Maybe Oid)
typeOidWithName typName = \encCtx -> typeOid <$> lookupTypeByName typName encCtx.typeInfoCache

instance ToPgField Int where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just haskellIntOid,
        toPgField = \_ -> binaryIntEncoder
      }

instance ToPgField Int16 where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just int2Oid,
        toPgField = \_ -> \n -> NotNull $ Cereal.encode n
      }

instance ToPgField Int32 where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just int4Oid,
        toPgField = \_ -> \n -> NotNull $ Cereal.encode n
      }

instance ToPgField Int64 where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just int8Oid,
        toPgField = \_ -> \n -> NotNull $ Cereal.encode n
      }

instance ToPgField Integer where
  fieldEncoder =
    let fe = fieldEncoder @Scientific
     in FieldEncoder
          { toTypeOid = \_ -> Just numericOid,
            toPgField = \encCtx -> \n -> fe.toPgField encCtx (fromIntegral n)
          }

instance ToPgField (Ratio Integer) where
  fieldEncoder =
    let fe = fieldEncoder @Scientific
     in FieldEncoder
          { toTypeOid = \_ -> Just numericOid,
            toPgField = \encCtx -> \r -> fe.toPgField encCtx (fromRational r)
          }

instance ToPgField Oid where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just oidOid,
        toPgField = \_ -> \n -> NotNull $ Cereal.encode @Int32 $ fromIntegral n
      }

instance ToPgField Scientific where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just numericOid,
        toPgField = \_ -> \n ->
          let sign = Cereal.encode @Int16 $ if n >= 0 then 0 else 0x4000
              -- The number is coeff * 10^exp, but we want it in base-10000 so we convert it to
              -- new_coeff * 10^new_exp with new_exp a multiple of 4
              base10000Expon = 4 * (base10Exponent n `div` 4)
              base10000Coeff = coefficient n * expt 10 (base10Exponent n - base10000Expon)
              ndigits, weight :: Int16
              digits :: ByteString
              (ndigits, weight, digits) = calculateDigits 0 0 (abs base10000Coeff) ""
              dscale = Cereal.encode @Int16 (abs $ fromIntegral base10000Expon) -- More than necessary, but safe?
           in NotNull $ Cereal.encode ndigits <> Cereal.encode (weight - 1 + fromIntegral (base10000Expon `div` 4)) <> sign <> dscale <> digits
      }
    where
      calculateDigits :: Int16 -> Int16 -> Integer -> BS.ByteString -> (Int16, Int16, BS.ByteString)
      calculateDigits !ndigitsSoFar !weightSoFar 0 !encodedDigits = (ndigitsSoFar, weightSoFar, encodedDigits)
      calculateDigits !ndigitsSoFar !weightSoFar !val !encodedDigits =
        let (quotient, fromIntegral -> rest :: Int16) = val `divMod` 10000
         in calculateDigits
              (ndigitsSoFar + 1)
              (weightSoFar + 1)
              quotient
              (Cereal.encode @Int16 rest <> encodedDigits)

instance ToPgField Float where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just float4Oid,
        toPgField = \_ -> \n -> NotNull $ Cereal.encode @Word32 $ castFloatToWord32 n
      }

instance ToPgField Double where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just float8Oid,
        toPgField = \_ -> \n -> NotNull $ Cereal.encode @Word64 $ castDoubleToWord64 n
      }

instance ToPgField Bool where
  -- TODO: Cereal.encode seems to work, but reference the documentation that shows how bools are encoded
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just boolOid,
        toPgField = \_ n -> NotNull $ Cereal.encode @Bool $ n
      }

instance ToPgField Day where
  -- PG Dates are Int32 number of days relative to 2000-01-01
  -- https://github.com/postgres/postgres/blob/master/src/include/datatype/timestamp.h#L235
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just dateOid,
        -- TODO: Catch integer overflow and do what?
        toPgField = \_ d -> NotNull $ Cereal.encode @Int32 $ fromIntegral $ diffDays d (fromGregorian 2000 1 1)
      }

instance ToPgField (Unbounded Day) where
  fieldEncoder =
    let fe = fieldEncoder @Day
     in FieldEncoder
          { toTypeOid = fe.toTypeOid,
            toPgField = \encCtx -> \case
              NegInfinity -> NotNull $ Cereal.encode @Int32 minBound
              Finite v -> fe.toPgField encCtx v
              PosInfinity -> NotNull $ Cereal.encode @Int32 maxBound
          }

instance ToPgField CalendarDiffTime where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just intervalOid,
        toPgField = \_ CalendarDiffTime {..} ->
          let (days :: Int32, timeUnderOneDay) = ctTime `divMod'` 86_400
           in NotNull $ Cereal.encode @(Int64, Int32, Int32) (round $ timeUnderOneDay * 1_000_000, days, fromIntegral ctMonths)
      }

instance ToPgField NominalDiffTime where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just intervalOid,
        toPgField = \_ ndt ->
          NotNull $ Cereal.encode @(Int64, Int32, Int32) (round $ ndt * 1_000_000, 0, 0)
      }

instance ToPgField UTCTime where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just timestamptzOid,
        -- TODO: Catch integer overflow and do what?
        toPgField = \_ (UTCTime parsedDate timeinday) ->
          let day :: Int64 = fromInteger $ parsedDate `diffDays` fromJulian 1999 12 19
              totalusecs :: Int64 = 86_400_000_000 * day + fromInteger (diffTimeToPicoseconds timeinday `div` 1_000_000)
           in NotNull $ Cereal.encode @Int64 totalusecs
      }

instance ToPgField (Unbounded UTCTime) where
  fieldEncoder =
    let fe = fieldEncoder @UTCTime
     in FieldEncoder
          { toTypeOid = fe.toTypeOid,
            toPgField = \encCtx -> \case
              NegInfinity -> NotNull $ Cereal.encode @Int64 minBound
              Finite v -> fe.toPgField encCtx v
              PosInfinity -> NotNull $ Cereal.encode @Int64 maxBound
          }

instance ToPgField ZonedTime where
  fieldEncoder =
    let fe = fieldEncoder @UTCTime
     in FieldEncoder
          { toTypeOid = \_ -> Just timestamptzOid,
            toPgField = \encCtx -> fe.toPgField encCtx . zonedTimeToUTC
          }

instance ToPgField (Unbounded ZonedTime) where
  fieldEncoder =
    let fe = fieldEncoder @ZonedTime
     in FieldEncoder
          { toTypeOid = fe.toTypeOid,
            toPgField = \encCtx -> \case
              NegInfinity -> NotNull $ Cereal.encode @Int64 minBound
              Finite v -> fe.toPgField encCtx v
              PosInfinity -> NotNull $ Cereal.encode @Int64 maxBound
          }

instance ToPgField LocalTime where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just timestampOid,
        toPgField = \_ (LocalTime localDay localTimeOfDay) ->
          let day :: Int64 = fromInteger $ localDay `diffDays` fromJulian 1999 12 19
              totalusecs :: Int64 = 86_400_000_000 * day + fromInteger (diffTimeToPicoseconds (timeOfDayToTime localTimeOfDay) `div` 1_000_000)
           in NotNull $ Cereal.encode @Int64 totalusecs
      }

instance ToPgField Char where
  fieldEncoder =
    let fe = fieldEncoder @Text
     in FieldEncoder
          { toTypeOid = \_ -> Just textOid,
            toPgField = \encCtx -> let !toTextField = fe.toPgField encCtx in \t -> toTextField $ Text.singleton t
          }

instance ToPgField ByteString where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just byteaOid,
        toPgField = \_ -> \bs -> NotNull bs
      }

instance ToPgField LBS.ByteString where
  fieldEncoder =
    let fe = fieldEncoder @ByteString
     in FieldEncoder
          { toTypeOid = \_ -> Just byteaOid,
            toPgField = \encCtx -> fe.toPgField encCtx . LBS.toStrict
          }

instance ToPgField Text where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just textOid,
        -- TODO: What about client_encoding?
        toPgField = \_ -> \t ->
          let bs = encodeUtf8 t
           in NotNull bs
      }

instance ToPgField LT.Text where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just textOid,
        -- TODO: What about client_encoding?
        toPgField = \_ -> \t ->
          let bs = LBS.toStrict $ LT.encodeUtf8 t
           in NotNull bs
      }

instance ToPgField String where
  fieldEncoder =
    let fe = fieldEncoder @Text
     in FieldEncoder
          { toTypeOid = \_ -> Just textOid,
            -- TODO: What about client_encoding?
            toPgField = \encCtx -> fe.toPgField encCtx . Text.pack
          }

-- From https://hackage.haskell.org/package/case-insensitive-1.2.1.0/docs/Data-CaseInsensitive.html,
-- "Note that the FoldCase instance for ByteStrings is only guaranteed to be correct for ISO-8859-1 encoded strings!".
-- So we don't have those instances.

-- | This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
-- connection option).
instance ToPgField (CI Text) where
  fieldEncoder = typeFieldEncoder (typeOidWithName "citext") $ contramap CI.original fieldEncoder

-- | This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
-- connection option).
instance ToPgField (CI LT.Text) where
  fieldEncoder = typeFieldEncoder (typeOidWithName "citext") $ contramap CI.original fieldEncoder

-- | This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
-- connection option).
instance ToPgField (CI String) where
  fieldEncoder = typeFieldEncoder (typeOidWithName "citext") $ contramap CI.original fieldEncoder

instance ToPgField UUID where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just uuidOid,
        toPgField = \_ -> NotNull . LBS.toStrict . UUID.toByteString
      }

instance ToPgField Aeson.Value where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just jsonbOid,
        toPgField = \_ -> \v ->
          let bs = BS.cons 1 (LBS.toStrict $ Aeson.encode v)
           in NotNull bs
      }

instance (ToPgField a) => ToPgField (Maybe a) where
  fieldEncoder =
    let fe = fieldEncoder @a
     in FieldEncoder
          { toTypeOid = fe.toTypeOid,
            toPgField = \encCtx -> \case
              Nothing -> SqlNull
              Just n -> fe.toPgField encCtx n
          }

-- | Returns a field-encoding function for a vector-like Foldable (e.g. Lists and Vector itself).
toPgVectorField :: forall f a. (Foldable f, ToPgField a) => EncodingContext -> f a -> BinaryField
toPgVectorField encCtx =
  let fe = fieldEncoder @a
      encodeElement el = Builder.binaryField $ fe.toPgField encCtx el
      Oid elemOid = fromMaybe (Oid 0) (fe.toTypeOid encCtx)
   in \vec ->
        let ndim = Builder.byteString $ Cereal.encode @Int32 1
            -- Postgres seems to build the "has_nulls" flag itself in the ReadArrayBinary function at https://github.com/postgres/postgres/blob/aa7f9493a02f5981c09b924323f0e7a58a32f2ed/src/backend/utils/adt/arrayfuncs.c#L1429, so we can just set it to 0
            hasNull = Builder.byteString $ Cereal.encode @Int32 0
            -- hasNull = Builder.byteString $ Cereal.encode @Int32 (if Vector.any (\e -> toPgField e == Nothing) vec then 1 else 0)
            elemOidBs = Builder.byteString $ Cereal.encode @Int32 elemOid
            lb1 = Builder.byteString $ Cereal.encode @Int32 1
            (Sum len, encodedElements) = foldMap (\el -> (Sum 1, encodeElement el)) vec
            dim1 = Builder.byteString $ Cereal.encode @Int32 len
            fullBs = ndim <> hasNull <> elemOidBs <> dim1 <> lb1 <> encodedElements
         in NotNull (Builder.toStrictByteString fullBs)

instance (ToPgField a) => ToPgField (Vector a) where
  fieldEncoder =
    let fe = fieldEncoder @a
     in FieldEncoder
          { toTypeOid = \encodingContext -> do
              -- Maybe monad
              elOid <- fe.toTypeOid encodingContext
              arrayTypInfo <- lookupTypeByOid elOid encodingContext.typeInfoCache
              arrayTypInfo.oidOfArrayType,
            toPgField = toPgVectorField
          }

data RowEncoder a = RowEncoder
  { toPgParams :: !(a -> [EncodingContext -> (Maybe Oid, BinaryField)]),
    toTypeOids :: !(Proxy a -> [EncodingContext -> Maybe Oid]),
    -- | This produces bytes for Binary COPY FROM STDIN rows, which can increase performance
    -- and reduce memory usage comparing to deriving these bytes from `toPgParams`.
    -- The produced bytes should not contain the total number of fields in the
    -- beginning.
    toBinaryCopyBytes :: !(EncodingContext -> a -> Builder.Builder)
  }

instance Contravariant RowEncoder where
  contramap f rec = RowEncoder (\v -> rec.toPgParams (f v)) (\_ -> rec.toTypeOids Proxy) (\encCtx -> let !toBytes = rec.toBinaryCopyBytes encCtx in \v -> toBytes (f v))

-- | These are from `Divisible`, but we don't currently pull in the extra dependency that has that.
divide :: (a -> (b, c)) -> RowEncoder b -> RowEncoder c -> RowEncoder a
divide d re1 re2 =
  RowEncoder
    { toPgParams = \a -> let (b, c) = d a in re1.toPgParams b ++ re2.toPgParams c,
      toTypeOids = \_ -> re1.toTypeOids Proxy ++ re2.toTypeOids Proxy,
      toBinaryCopyBytes = \encCtx ->
        let !toBytes1 = re1.toBinaryCopyBytes encCtx
            !toBytes2 = re2.toBinaryCopyBytes encCtx
         in \a -> let (b, c) = d a in toBytes1 b <> toBytes2 c
    }

class ToPgRow a where
  rowEncoder :: RowEncoder a
  default rowEncoder :: (Generic a, ProductTypeEncoder (Rep a)) => RowEncoder a
  rowEncoder = genericToPgRow

instance ToPgRow () where
  rowEncoder = RowEncoder (\_ -> []) (\_ -> []) (\_ -> \_ -> mempty)

singleFieldRowEncoder :: forall a. (ToPgField a) => RowEncoder a
singleFieldRowEncoder =
  let fe = fieldEncoder @a
   in RowEncoder
        { toPgParams = \a -> [\encodingContext -> (fe.toTypeOid encodingContext, fe.toPgField encodingContext a)],
          toTypeOids = \_ -> [fe.toTypeOid],
          toBinaryCopyBytes = \encCtx -> let !enc = fe.toPgField encCtx in \a -> Builder.binaryField $ enc a
        }

instance (ToPgField a) => ToPgRow (Only a) where
  rowEncoder = contramap fromOnly singleFieldRowEncoder

instance (ToPgField a, ToPgField b) => ToPgRow (a, b) where
  rowEncoder = divide id singleFieldRowEncoder singleFieldRowEncoder

instance (ToPgField a, ToPgField b, ToPgField c) => ToPgRow (a, b, c) where
  rowEncoder = divide (\(a, b, c) -> ((a, b), c)) rowEncoder singleFieldRowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d) => ToPgRow (a, b, c, d) where
  rowEncoder = divide (\(a, b, c, d) -> ((a, b), (c, d))) rowEncoder rowEncoder

-- This instance implements toBinaryCopyBytes as well because we did this
-- to test if this method can help improve performance of COPY in our
-- benchmarks. We found that it can, but we didn't bother yet implementing
-- this for other types.
-- toBinaryCopyBytes encCtx = \(a, b, c, d) -> Builder.int16BE 4 <> toPgFieldWithSize a <> toPgFieldWithSize b <> toPgFieldWithSize c <> toPgFieldWithSize d
--   where
--     toPgFieldWithSize :: (ToPgField x) => x -> Builder.Builder
--     toPgFieldWithSize v = Builder.binaryField $ toPgField encCtx v

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e) => ToPgRow (a, b, c, d, e) where
  rowEncoder = divide (\(a, b, c, d, e) -> ((a, b, c), (d, e))) rowEncoder rowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f) => ToPgRow (a, b, c, d, e, f) where
  rowEncoder = divide (\(a, b, c, d, e, f) -> ((a, b, c), (d, e, f))) rowEncoder rowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g) => ToPgRow (a, b, c, d, e, f, g) where
  rowEncoder = divide (\(a, b, c, d, e, f, g) -> ((a, b, c), (d, e, f, g))) rowEncoder rowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h) => ToPgRow (a, b, c, d, e, f, g, h) where
  rowEncoder = divide (\(a, b, c, d, e, f, g, h) -> ((a, b, c, d), (e, f, g, h))) rowEncoder rowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i) => ToPgRow (a, b, c, d, e, f, g, h, i) where
  rowEncoder = divide (\(a, b, c, d, e, f, g, h, i) -> ((a, b, c, d), (e, f, g, h, i))) rowEncoder rowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i, ToPgField j) => ToPgRow (a, b, c, d, e, f, g, h, i, j) where
  rowEncoder = divide (\(a, b, c, d, e, f, g, h, i, j) -> ((a, b, c, d, e), (f, g, h, i, j))) rowEncoder rowEncoder

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i, ToPgField j, ToPgField k) => ToPgRow (a, b, c, d, e, f, g, h, i, j, k) where
  rowEncoder = divide (\(a, b, c, d, e, f, g, h, i, j, k) -> ((a, b, c, d, e, f), (g, h, i, j, k))) rowEncoder rowEncoder

-- instance (ToPgField a) => ToPgRow [a] where
--   rowEncoder = RowEncoder {
--     toPgParams = \xs -> concatMap toPgParams xs
--     , toTypeOids = \_ -> concatMap (\)
--   } $ \cols -> map (\v encodingContext -> let typOid = toTypeOid (Proxy @a) encodingContext in (typOid, toPgField encodingContext v)) cols

-- | A way to compose rows.
data h :. t = !h :. !t deriving (Eq, Ord, Show, Read)

infixr 3 :.

instance forall a b. (ToPgRow a, ToPgRow b) => ToPgRow (a :. b) where
  rowEncoder =
    let !re1 = rowEncoder @a
        !re2 = rowEncoder @b
     in RowEncoder
          { toPgParams = \(a :. b) -> re1.toPgParams a ++ re2.toPgParams b,
            toTypeOids = \_ -> re1.toTypeOids (Proxy @a) ++ re2.toTypeOids (Proxy @b),
            toBinaryCopyBytes = \encCtx ->
              let !toBytes1 = re1.toBinaryCopyBytes encCtx
                  !toBytes2 = re2.toBinaryCopyBytes encCtx
               in \(a :. b) -> toBytes1 a <> toBytes2 b
          }

instance (FromPgRow a, FromPgRow b) => FromPgRow (a :. b) where
  rowDecoder = (:.) <$> rowDecoder <*> rowDecoder

-- | The OID for `Data.Int`, which is machine dependent.
haskellIntOid :: Oid

-- | All pg type OIDs that fit into Haskell's `Data.Int`, whose size is machine dependent.
haskellIntOids :: [Oid]
(haskellIntOid, haskellIntOids)
  | (fromIntegral (maxBound @Int) :: Integer) > fromIntegral (maxBound @Int32) = (int8Oid, [int2Oid, int4Oid, int8Oid])
  | (fromIntegral (maxBound @Int) :: Integer) > fromIntegral (maxBound @Int16) = (int4Oid, [int2Oid, int4Oid])
  | otherwise = (int2Oid, [int2Oid])

-- | Big-Endian binary encoder for Haskell's `Data.Int`, which is machine-dependent.
binaryIntEncoder :: Int -> BinaryField
binaryIntEncoder
  | haskellIntOid == int8Oid = NotNull . Cereal.encode @Int64 . fromIntegral
  | haskellIntOid == int4Oid = NotNull . Cereal.encode @Int32 . fromIntegral
  | otherwise = NotNull . Cereal.encode @Int16 . fromIntegral

-- | Big-Endian binary decoder for Haskell's various IntXX types.
binaryIntDecoder :: forall a. (Integral a, Bounded a) => Oid -> ByteString -> Either String a
binaryIntDecoder typOid = \bs ->
  if doesFit
    then intDecoder bs
    else Left $ "Chosen integral type does not fit every value for PG type with OID " ++ show typOid
  where
    maxBoundPgType :: Integer
    intDecoder :: ByteString -> Either String a
    (maxBoundPgType, intDecoder)
      | typOid == int8Oid = (fromIntegral $ maxBound @Int64, fmap fromIntegral . Cereal.decode @Int64)
      | typOid == int4Oid = (fromIntegral $ maxBound @Int32, fmap fromIntegral . Cereal.decode @Int32)
      | typOid == int2Oid = (fromIntegral $ maxBound @Int16, fmap fromIntegral . Cereal.decode @Int16)
      | otherwise = error "Bug in Hpgsql. Decoding binary integral type not an int2, int4 or int8"
    doesFit = maxBoundPgType <= fromIntegral (maxBound @a)

binaryFloat4Decoder :: ByteString -> Float
binaryFloat4Decoder = castWord32ToFloat . either error id . Cereal.decode @Word32

binaryFloat8Decoder :: ByteString -> Double
binaryFloat8Decoder = castWord64ToDouble . either error id . Cereal.decode @Word64

parsePgType :: [Oid] -> (Maybe ByteString -> Either String a) -> FieldDecoder a
parsePgType !requiredTypeOids !fieldValueDecoder =
  FieldDecoder
    { fieldValueDecoder = \_oid -> fieldValueDecoder,
      allowedPgTypes = (`elem` requiredTypeOids) . fieldTypeOid
    }

instance FromPgField () where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \_oid -> \case
          Just "" -> Right ()
          Just bs -> Left $ "Invalid value '" ++ show bs ++ "' for postgres void type"
          Nothing -> Left "Cannot decode SQL null as the Haskell () type. Use a `Maybe ()`",
        allowedPgTypes = (== voidOid) . fieldTypeOid
      }

instance FromPgField Int where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int type. Use a `Maybe Int`",
        allowedPgTypes = (`elem` haskellIntOids) . fieldTypeOid
      }

instance FromPgField Int16 where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int16 type. Use a `Maybe Int16`",
        allowedPgTypes = (== int2Oid) . fieldTypeOid
      }

instance FromPgField Int32 where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int32 type. Use a `Maybe Int32`",
        allowedPgTypes = (`elem` [int2Oid, int4Oid]) . fieldTypeOid
      }

instance FromPgField Int64 where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int64 type. Use a `Maybe Int64`",
        allowedPgTypes = (`elem` [int2Oid, int4Oid, int8Oid]) . fieldTypeOid
      }

instance FromPgField Integer where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decodeInt = binaryIntDecoder @Int64 oid
           in \case
                Just bs
                  | oid /= numericOid -> fromIntegral <$> decodeInt bs
                  | otherwise -> case Parser.parseOnly (scientificDecoder True <* Parser.endOfInput) bs of
                      Parser.ParseOk sci -> case floatingOrInteger @Double @Integer sci of
                        Right i -> Right i
                        Left _ -> Left "Internal error in Hpgsql. Scientific to Integer conversion failed"
                      Parser.ParseFail err -> Left err
                Nothing -> Left "Cannot decode SQL null as the Haskell Integer type. Use a `Maybe Integer`",
        allowedPgTypes = (`elem` [int8Oid, numericOid, int4Oid, int2Oid]) . fieldTypeOid
      }

instance FromPgField Oid where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \_ -> \case
          -- Oids are just int4
          Just bs -> Oid <$> binaryIntDecoder int4Oid bs
          Nothing -> Left "Cannot decode SQL null as the Haskell Oid type. Use a `Maybe Oid`",
        allowedPgTypes = (== oidOid) . fieldTypeOid
      }

instance FromPgField Float where
  fieldDecoder = parsePgType [float4Oid] $ \case
    Just bs -> Right $ binaryFloat4Decoder bs
    Nothing -> Left "Cannot decode SQL null as the Haskell Float type. Use a `Maybe Float`"

instance FromPgField Double where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decoder
                | oid == float8Oid = binaryFloat8Decoder
                | otherwise = float2Double . binaryFloat4Decoder
           in \case
                Just bs -> Right $ decoder bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Double type. Use a `Maybe Double`",
        allowedPgTypes = (`elem` [float8Oid, float4Oid]) . fieldTypeOid
      }

-- | A decoder that accepts any PG type and returns the object's
-- postgres' binary representation as a ByteString.
-- This can be useful to build `FromPgField` instances for enum types,
-- since postgres uses their UTF8 text representation even in the
-- binary protocol, but otherwise it's easier to compose existing
-- FieldDecoders.
rawBytesFieldDecoder :: FieldDecoder ByteString
rawBytesFieldDecoder =
  FieldDecoder
    { fieldValueDecoder = \_oid -> \case
        Nothing -> Left "Cannot decode SQL null as the `rawBytesFieldDecoder`."
        Just bs -> Right bs,
      allowedPgTypes = const True
    }

-- | Allows you to specify a type (and other checks, possibly) for a FieldDecoder.
-- This can be useful to ensure you're not accidentally decoding a different type.
--
-- > data MyEnum = Val1 | Val2 | Val3
-- > myEnumFieldDecoderWithTypeInfoCheck :: FieldDecoder MyEnum
-- > myEnumFieldDecoderWithTypeInfoCheck =
-- >   let convert = \case
-- >         "val1" -> Val1
-- >         "val2" -> Val2
-- >         "val3" -> Val3
-- >         _ -> error "Invalid value for MyEnum"
-- >    in typeFieldDecoder
-- >         (typeMustBeNamed "my_enum")
-- >         $ convert <$> rawBytesFieldDecoder
--
-- This will work unless you use non-default flags in your connection options.
typeFieldDecoder :: (ColumnInfo -> Bool) -> FieldDecoder a -> FieldDecoder a
typeFieldDecoder fieldCheck dec = dec {allowedPgTypes = fieldCheck}

typeMustBeNamed :: Text -> (ColumnInfo -> Bool)
typeMustBeNamed typName = \fieldInfo ->
  (typeName <$> lookupTypeByOid fieldInfo.fieldTypeOid fieldInfo.encodingContext.typeInfoCache) == Just typName

scientificDecoder :: Bool -> Parser.Parser Scientific
scientificDecoder mustBeInteger = do
  ndigits <- int16Parser
  weight <- int16Parser
  sign <- int16Parser -- 0x0000 is positive, 0x4000 is negative, 0xC000 is NAN, 0xD000 is Positive Infinity, 0xF000 is Negative Infinity
  unless (sign == 0x0000 || sign == 0x4000) $ fail "NaN, positive or negative infinities cannot be decoded into Integer or Scientific"
  !dscale <- int16Parser
  when (mustBeInteger && dscale /= 0) $ fail "Decoding into `Integer` requires explicit casting with `numeric(X,0)` to force integral values"
  valueAbs <- parseAndMult ndigits (fromIntegral weight * 4) 0
  pure $ (if sign == 0x0000 then 1 else (-1)) * valueAbs
  where
    parseAndMult :: Int16 -> Int -> Scientific -> Parser.Parser Scientific
    parseAndMult 0 _ !val = pure val
    parseAndMult !ndigitsLeft !currexpon !val = do
      !digit <- fromIntegral <$> int16Parser
      parseAndMult (ndigitsLeft - 1) (currexpon - 4) (val + scientific digit currexpon)

instance FromPgField Scientific where
  -- See https://github.com/postgres/postgres/blob/799959dc7cf0e2462601bea8d07b6edec3fa0c4f/src/backend/utils/adt/numeric.c#L1163
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \ColumnInfo {fieldTypeOid = oid} ->
          let !decodeInt = binaryIntDecoder @Int64 oid
           in \case
                Just bs ->
                  -- TODO: There is loss converting from Float/Double to Scientific, but it might be quite small, so should we accept
                  -- float4Oid and float8Oid here?
                  if oid == numericOid
                    then case Parser.parseOnly (scientificDecoder False <* Parser.endOfInput) bs of
                      Parser.ParseOk sci -> Right sci
                      Parser.ParseFail err -> Left err
                    else flip scientific 0 . fromIntegral <$> decodeInt bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Scientific type. Use a `Maybe Scientific`",
        allowedPgTypes = (`elem` [numericOid, int2Oid, int4Oid, int8Oid]) . fieldTypeOid
      }

instance FromPgField (Ratio Integer) where
  fieldDecoder = toRational <$> fieldDecoder @Scientific

binaryTrue :: ByteString
binaryTrue = Cereal.encode True

instance FromPgField Bool where
  fieldDecoder = parsePgType [boolOid] $ \case
    Just bs -> Right $ bs == binaryTrue
    Nothing -> Left "Cannot decode SQL null as the Haskell Bool type. Use a `Maybe Bool`"

instance FromPgField Char where
  fieldDecoder =
    let textParser = fieldValueDecoder (fieldDecoder @Text)
     in FieldDecoder
          { fieldValueDecoder = \colInfo@ColumnInfo {fieldTypeOid = oid} ->
              let !decodeText = textParser colInfo
               in \mbs -> case mbs of
                    Just bs ->
                      if oid == charOid
                        -- TODO: Postgres has values of type "char" in the pg_type.typcategory table.
                        -- We should test this instance works with those, and we haven't yet.
                        then Right $ BSC.head bs
                        else case decodeText mbs of
                          Left err -> Left err
                          Right t -> if Text.length t > 1 then Left "Cannot parse text with more than one character into a Haskell Char type." else Right (Text.head t)
                    Nothing -> Left "Cannot decode SQL null as the Haskell Char type. Use a `Maybe Char`",
            -- TODO: All the varchar types?
            allowedPgTypes = (`elem` [charOid, textOid]) . fieldTypeOid
          }

instance FromPgField ByteString where
  fieldDecoder = parsePgType [byteaOid] $ \case
    Just bs -> Right bs
    Nothing -> Left "Cannot decode SQL null as the Haskell ByteString type. Use a `Maybe ByteString`"

instance FromPgField LBS.ByteString where
  fieldDecoder = parsePgType [byteaOid] $ \case
    Just bs -> Right $ LBS.fromStrict bs
    Nothing -> Left "Cannot decode SQL null as the Haskell ByteString type. Use a `Maybe ByteString`"

instance FromPgField Text where
  fieldDecoder = parsePgType [textOid, varcharOid, nameOid] $ \case
    Just bs -> Right $ decodeUtf8 bs -- TODO: Ensure we set client_encoding=utf8 in our connections!
    -- TODO: Use some faster unsafeDecodeUtf8 function?
    Nothing -> Left "Cannot decode SQL null as the Haskell Text type. Use a `Maybe Text`"

instance FromPgField LT.Text where
  fieldDecoder = parsePgType [textOid, varcharOid, nameOid] $ \case
    Just bs -> Right $ LT.fromStrict $ decodeUtf8 bs -- TODO: Ensure we set client_encoding=utf8 in our connections!
    -- TODO: Use some faster unsafeDecodeUtf8 function?
    Nothing -> Left "Cannot decode SQL null as the Haskell Text type. Use a `Maybe Text`"

instance FromPgField String where
  fieldDecoder = parsePgType [textOid, varcharOid, nameOid] $ \case
    -- \| This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
    -- connection option).
    Just bs -> Right $ Text.unpack $ decodeUtf8 bs -- TODO: Ensure we set client_encoding=utf in our connections!
    -- TODO: Use some faster unsafeDecodeUtf8 function?
    Nothing -> Left "Cannot decode SQL null as the Haskell String type. Use a `Maybe String`"

-- | This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
-- connection option).
instance FromPgField (CI Text) where
  fieldDecoder = typeFieldDecoder (typeMustBeNamed "citext") $ CI.mk <$> fieldDecoder

-- | This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
-- connection option).
instance FromPgField (CI LT.Text) where
  fieldDecoder = typeFieldDecoder (typeMustBeNamed "citext") $ CI.mk <$> fieldDecoder

-- | This instance does not work if you have fillTypeInfoCache disabled (that would be a non-default
-- connection option).
instance FromPgField (CI String) where
  fieldDecoder = typeFieldDecoder (typeMustBeNamed "citext") $ CI.mk <$> fieldDecoder

instance FromPgField UTCTime where
  fieldDecoder = parsePgType [timestamptzOid] $ \case
    Just bs -> do
      -- See https://github.com/postgres/postgres/blob/50cb7505b3010736b9a7922e903931534785f3aa/src/backend/utils/adt/timestamp.c#L1909
      totalusecs <- Cereal.decode @Int64 bs
      let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
          parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
      Right $ UTCTime parsedDate (picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell UTCTime type. Use a `Maybe UTCTime`"

instance FromPgField (Unbounded UTCTime) where
  fieldDecoder = parsePgType [timestamptzOid] $ \case
    Just bs -> do
      -- See https://github.com/postgres/postgres/blob/50cb7505b3010736b9a7922e903931534785f3aa/src/backend/utils/adt/timestamp.c#L1909
      totalusecs <- Cereal.decode @Int64 bs
      Right $
        if totalusecs == minBound
          then NegInfinity
          else
            if totalusecs == maxBound
              then PosInfinity
              else
                let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
                    parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
                 in Finite $ UTCTime parsedDate (picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell (Unbounded UTCTime) type. Use a `Maybe (Unbounded UTCTime)`"

instance FromPgField ZonedTime where
  fieldDecoder = parsePgType [timestamptzOid] $ \case
    Just bs -> do
      -- See https://github.com/postgres/postgres/blob/50cb7505b3010736b9a7922e903931534785f3aa/src/backend/utils/adt/timestamp.c#L1909
      totalusecs <- Cereal.decode @Int64 bs
      let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
          parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
      Right $ utcToZonedTime utc $ UTCTime parsedDate (picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell ZonedTime type. Use a `Maybe ZonedTime`"

instance FromPgField (Unbounded ZonedTime) where
  fieldDecoder = parsePgType [timestamptzOid] $ \case
    Just bs -> do
      -- See https://github.com/postgres/postgres/blob/50cb7505b3010736b9a7922e903931534785f3aa/src/backend/utils/adt/timestamp.c#L1909
      totalusecs <- Cereal.decode @Int64 bs
      Right $
        if totalusecs == minBound
          then NegInfinity
          else
            if totalusecs == maxBound
              then PosInfinity
              else
                let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
                    parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
                 in Finite $ utcToZonedTime utc $ UTCTime parsedDate (picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell ZonedTime type. Use a `Maybe ZonedTime`"

instance FromPgField LocalTime where
  fieldDecoder = parsePgType [timestampOid] $ \case
    Just bs -> do
      totalusecs <- Cereal.decode @Int64 bs
      let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
          parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
      Right $ LocalTime parsedDate (timeToTimeOfDay $ picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell LocalTime type. Use a `Maybe LocalTime`"

instance FromPgField Day where
  fieldDecoder = parsePgType [dateOid] $ \case
    Just bs -> do
      -- There is a very specific conversion function for these, which I poorly translated to Haskell
      -- https://github.com/postgres/postgres/blob/799959dc7cf0e2462601bea8d07b6edec3fa0c4f/src/backend/utils/adt/datetime.c#L321
      -- But I found a simpler way to do this. Let's see if it works in our property based tests
      jd <- Cereal.decode @Int32 bs
      Right $ addJulianDurationClip (CalendarDiffDays 0 (fromIntegral jd - 13)) $ fromJulian 2000 01 01
    Nothing -> Left "Cannot decode SQL null as the Haskell Day type. Use a `Maybe Day`"

instance FromPgField (Unbounded Day) where
  fieldDecoder = parsePgType [dateOid] $ \case
    Just bs -> do
      -- There is a very specific conversion function for these, which I poorly translated to Haskell
      -- https://github.com/postgres/postgres/blob/799959dc7cf0e2462601bea8d07b6edec3fa0c4f/src/backend/utils/adt/datetime.c#L321
      -- But I found a simpler way to do this. Let's see if it works in our property based tests
      jd <- Cereal.decode @Int32 bs
      Right $
        if jd == minBound
          then NegInfinity
          else
            if jd == maxBound
              then PosInfinity
              else
                Finite $ addJulianDurationClip (CalendarDiffDays 0 (fromIntegral jd - 13)) $ fromJulian 2000 01 01
    Nothing -> Left "Cannot decode SQL null as the Haskell (Unbounded Day) type. Use a `Maybe (Unbounded Day)`"

instance FromPgField CalendarDiffTime where
  fieldDecoder = parsePgType [intervalOid] $ \case
    Just bs -> do
      (nMicrosecs :: Int64, nDays :: Int32, nMonths :: Int32) <- Cereal.decode bs
      Right $ CalendarDiffTime {ctMonths = fromIntegral nMonths, ctTime = secondsToNominalDiffTime (fromIntegral nDays * 86400) + realToFrac (picosecondsToDiffTime (fromIntegral nMicrosecs * 1_000_000))}
    Nothing -> Left "Cannot decode SQL null as the Haskell CalendarDiffTime type. Use a `Maybe CalendarDiffTime`"

instance FromPgField UUID where
  fieldDecoder = parsePgType [uuidOid] $ \case
    Just bs -> case UUID.fromByteString (LBS.fromStrict bs) of
      Just uuid -> Right uuid
      Nothing -> Left "Bug in Hpgsql: UUID field could not be decoded"
    Nothing -> Left "Cannot decode SQL null as the Haskell UUID type. Use a `Maybe UUID`"

instance FromPgField Aeson.Value where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder =
          \ColumnInfo {fieldTypeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if fieldTypeOid == jsonbOid then BS.drop 1 else Prelude.id
             in \case
                  Just bs -> case Aeson.decodeStrict $ fixJsonb bs of
                    Just d -> Right d
                    Nothing -> Left "Bug in Hpgsql. Postgres produced a json or jsonb value that Aeson does not consider valid."
                  Nothing -> Left "Cannot decode SQL null as the Haskell Aeson.Value type. Use a `Maybe Aeson.Value` if you want SQL nulls",
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . fieldTypeOid
      }

-- | A FieldDecoder that accepts and decodes SQL NULLs into `Nothing` values
-- for a given decoder.
nullableField :: FieldDecoder a -> FieldDecoder (Maybe a)
nullableField FieldDecoder {..} =
  FieldDecoder
    { fieldValueDecoder = \oid ->
        let !origFieldValueParser = fieldValueDecoder oid
         in \case
              Nothing -> Right Nothing
              justBs -> Just <$> origFieldValueParser justBs,
      allowedPgTypes
    }

instance (FromPgField a) => FromPgField (Maybe a) where
  fieldDecoder = nullableField fieldDecoder

-- | A FieldDecoder that accepts and decodes Postgres arrays.
arrayField :: forall a f. (Monoid (f a)) => (forall m. (Monad m) => Int -> m a -> m (f a)) -> FieldDecoder a -> FieldDecoder (f a)
arrayField !replicateFunction !elementParser =
  -- From https://github.com/postgres/postgres/blob/5941946d0934b9eccb0d5bfebd40b155249a0130/src/backend/utils/adt/arrayfuncs.c#L1548
  FieldDecoder
    { fieldValueDecoder = \colInfo ->
        let !arrayFieldDecoder = arrayParser colInfo.encodingContext <* Parser.endOfInput
         in \case
              Nothing -> Left "Cannot decode SQL null as the Haskell Vector type. Use a `Maybe (Vector a)`"
              Just bs -> case Parser.parseOnly arrayFieldDecoder bs of
                Parser.ParseOk v -> Right v
                Parser.ParseFail err -> Left err,
      allowedPgTypes = const True -- TODO: We could put "Is-Array" in the typeinfo cache and reject when it's not an array here
    }
  where
    arrayParser :: EncodingContext -> Parser.Parser (f a)
    arrayParser encodingContext = do
      !ndim <- int32Parser
      !_hasNull <- int32Parser
      !elementTypeOid :: Oid <- Oid . fromIntegral <$> int32Parser
      let !elementColInfo = ColumnInfo elementTypeOid encodingContext
      when (ndim > 1) $ fail $ "TODO: No support for multi-dimensional arrays in Hpgsql. Got array with ndim=" ++ show ndim
      if ndim == 0
        then pure mempty
        else do
          !dim_i :: Int <- fromIntegral <$> int32Parser
          !_lb_i <- int32Parser
          unless (elementParser.allowedPgTypes elementColInfo) $ fail $ "Array contains elements of type OID " ++ show elementTypeOid ++ " but decoder does not handle that type"
          replicateFunction dim_i $ do
            size :: Int <- fromIntegral <$> int32Parser
            elementBs <- if size == (-1) then pure Nothing else Just <$> Parser.take size
            case elementParser.fieldValueDecoder elementColInfo elementBs of
              Left err -> fail $ "Error parsing array element: " ++ show err
              Right el -> pure el

instance forall a. (FromPgField a) => FromPgField (Vector a) where
  fieldDecoder = arrayField Vector.replicateM fieldDecoder

instance {-# OVERLAPPING #-} forall a. (FromPgField a) => FromPgField (Vector (Vector a)) where
  -- From https://github.com/postgres/postgres/blob/5941946d0934b9eccb0d5bfebd40b155249a0130/src/backend/utils/adt/arrayfuncs.c#L1548
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder = \colInfo ->
          let !arrayFieldDecoder = arrayParser colInfo.encodingContext <* Parser.endOfInput
           in \case
                Nothing -> Left "Cannot decode SQL null as the Haskell Vector type. Use a `Maybe (Vector (Vector a))`"
                Just bs -> case Parser.parseOnly arrayFieldDecoder bs of
                  Parser.ParseOk v -> Right v
                  Parser.ParseFail err -> Left err,
        allowedPgTypes = const True -- TODO: We could put "Is-Array" in the typeinfo cache and reject when it's not an array here
      }
    where
      !elementParser = fieldDecoder @a
      arrayParser :: EncodingContext -> Parser.Parser (Vector (Vector a))
      arrayParser encodingContext = do
        !ndim <- int32Parser
        !_hasNull <- int32Parser
        !elementTypeOid :: Oid <- Oid . fromIntegral <$> int32Parser
        let !elementColInfo = ColumnInfo elementTypeOid encodingContext
        when (ndim /= 2) $ fail $ "TODO: No support for " ++ show ndim ++ "-dimensional arrays in Hpgsql. Got array with ndim=" ++ show ndim
        unless (elementParser.allowedPgTypes elementColInfo) $ fail $ "Array contains elements of type OID " ++ show elementTypeOid ++ " but decoder does not handle that type"
        -- TODO: Check binary/text compatibility somehow? No, easier to get rid of TextFmt once and for all
        numRows <- do
          !dim_i :: Int <- fromIntegral <$> int32Parser
          !_lb_i <- int32Parser
          pure dim_i
        lengthEachRow <- do
          !dim_i :: Int <- fromIntegral <$> int32Parser
          !_lb_i <- int32Parser
          pure dim_i

        Vector.replicateM numRows $ do
          Vector.replicateM lengthEachRow $
            do
              size :: Int <- fromIntegral <$> int32Parser
              elementBs <- if size == (-1) then pure Nothing else Just <$> Parser.take size
              case elementParser.fieldValueDecoder elementColInfo elementBs of
                Left err -> fail $ "Error parsing array element: " ++ show err
                Right el -> pure el

int16Parser :: Parser.Parser Int16
int16Parser = either fail pure . Cereal.decode @Int16 =<< Parser.take 2

-- {- Generic deriving -}
genericFromPgRow :: forall a. (Generic a, ProductTypeDecoder (Rep a)) => RowDecoder a
genericFromPgRow = to <$> genRowDecoder @(Rep a)

class ProductTypeDecoder f where
  genRowDecoder :: RowDecoder (f a)

instance (ProductTypeDecoder a, ProductTypeDecoder b) => ProductTypeDecoder (a :*: b) where
  genRowDecoder = (:*:) <$> genRowDecoder <*> genRowDecoder

instance (ProductTypeDecoder f) => ProductTypeDecoder (M1 a c f) where
  genRowDecoder = M1 <$> genRowDecoder

instance (FromPgField a) => ProductTypeDecoder (K1 r a) where
  genRowDecoder = fmap K1 $ singleField $ fieldDecoder @a

genericToPgRow :: forall a. (Generic a, ProductTypeEncoder (Rep a)) => RowEncoder a
genericToPgRow = contramap from genRowEncoder

class ProductTypeEncoder f where
  genRowEncoder :: RowEncoder (f a)

instance (ProductTypeEncoder a, ProductTypeEncoder b) => ProductTypeEncoder (a :*: b) where
  genRowEncoder = divide (\(a :*: b) -> (a, b)) genRowEncoder genRowEncoder

instance (ProductTypeEncoder f) => ProductTypeEncoder (M1 i c f) where
  genRowEncoder = contramap unM1 genRowEncoder

instance (ToPgField a) => ProductTypeEncoder (K1 r a) where
  genRowEncoder = contramap unK1 singleFieldRowEncoder

newtype LowerCasedPgEnum a = LowerCasedPgEnum a

instance (Generic a, EnumDecoder (Rep a)) => FromPgField (LowerCasedPgEnum a) where
  fieldDecoder = LowerCasedPgEnum <$> genericEnumFieldDecoder LT.toLower

instance (Generic a, EnumEncoder (Rep a)) => ToPgField (LowerCasedPgEnum a) where
  fieldEncoder = untypedFieldEncoder $ \_encCtx -> \(LowerCasedPgEnum v) -> NotNull $ genericEnumToPgField Text.toLower v

genericEnumFieldDecoder ::
  forall a.
  (Generic a, EnumDecoder (Rep a)) =>
  -- | A function that takes in the Haskell constructor name and returns the textual representation of the enum in postgres
  (LT.Text -> LT.Text) ->
  FieldDecoder a
genericEnumFieldDecoder nameTransform = fromMaybe (error $ "Invalid enum value. Not one of " ++ show (Map.keys allValuesMap)) . flip Map.lookup allValuesMap <$> rawBytesFieldDecoder
  where
    -- TODO: Vector of pointers to ByteStrings for a bit more memory locality? Does it make a perf difference?
    allValuesMap = Map.mapKeys (LBS.toStrict . LT.encodeUtf8 . nameTransform) $ fmap to genEnumDecoder

class EnumDecoder f where
  -- | Returns the textual representation and constructed object for every possible
  -- value of the enum.
  genEnumDecoder :: Map LT.Text (f a)

instance (EnumDecoder a, EnumDecoder b) => EnumDecoder (a :+: b) where
  genEnumDecoder = (L1 <$> genEnumDecoder) `Map.union` (R1 <$> genEnumDecoder)

instance (EnumDecoder f) => EnumDecoder (M1 D c f) where
  genEnumDecoder = M1 <$> genEnumDecoder

-- U1 is "Unit"-type, that is: no value in the constructor, AKA "pure enum".
instance (KnownSymbol ctorName) => EnumDecoder (M1 C ('MetaCons ctorName ctorFixity 'False) U1) where
  genEnumDecoder = Map.singleton (LT.pack $ symbolVal (Proxy @ctorName)) (M1 U1)

genericEnumToPgField ::
  forall a.
  (Generic a, EnumEncoder (Rep a)) =>
  -- | A function that takes in the Haskell constructor name and returns the textual representation of the enum in postgres
  (Text -> Text) ->
  a ->
  ByteString
genericEnumToPgField nameTransform = encodeUtf8 . nameTransform . genEnumEncoder . from

class EnumEncoder f where
  -- | Returns the textual representation of an enum value's constructor.
  genEnumEncoder :: f a -> Text

instance (EnumEncoder a, EnumEncoder b) => EnumEncoder (a :+: b) where
  genEnumEncoder (L1 x) = genEnumEncoder x
  genEnumEncoder (R1 x) = genEnumEncoder x

instance (EnumEncoder f) => EnumEncoder (M1 D c f) where
  genEnumEncoder (M1 x) = genEnumEncoder x

-- U1 is "Unit"-type, that is: no value in the constructor, AKA "pure enum".
instance (KnownSymbol ctorName) => EnumEncoder (M1 C ('MetaCons ctorName ctorFixity 'False) U1) where
  genEnumEncoder _ = Text.pack $ symbolVal (Proxy @ctorName)
