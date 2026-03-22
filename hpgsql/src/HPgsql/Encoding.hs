{-# LANGUAGE UndecidableInstances #-}

module HPgsql.Encoding
  ( FromPgRow (..),
    FromPgField (..),
    ToPgField (..),
    ToPgRow (..),
    Only (..),
    ColumnInfo (..),
    FieldParser (..), -- TODO: Can we export ctor?
    RowParser (..), -- TODO: Can we export ctor?
    AllowNull (..),
    LowerCasedPgEnum (..),
    anyTypeDecoder,
    singleColRowParser,
    haskellIntOid,
    haskellIntOids,
    binaryIntDecoder,
    binaryIntEncoder,
    -- TODO: Methods below should be internal
    parsePgType,
    compositeTypeParser,
  )
where

import Control.Monad (replicateM, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Lazy as Parsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8
import Data.Fixed (divMod')
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific (..), floatingOrInteger, scientific)
import qualified Data.Serialize as Cereal
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Time (CalendarDiffDays (..), CalendarDiffTime (..), Day, UTCTime (..), ZonedTime, diffDays, diffTimeToPicoseconds, fromGregorian, picosecondsToDiffTime, secondsToNominalDiffTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Calendar.Julian (addJulianDurationClip, fromJulian)
import Data.Tuple.Only (Only (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble, expt, float2Double)
import GHC.Generics (C, D, Generic (..), K1 (..), M1 (..), Meta (MetaCons), U1 (..), (:*:) (..), (:+:) (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import HPgsql.TypeInfo (Format (..), Oid (..), TypeInfo, boolOid, byteaOid, charOid, dateOid, float4Oid, float8Oid, int2Oid, int4Oid, int8Oid, intervalOid, jsonOid, jsonbOid, nameOid, numericOid, oidOid, textOid, timestamptzOid, varcharOid, voidOid)

data ColumnInfo = ColumnInfo
  { typeOid :: !Oid,
    -- | The TypeInfo cache as of the moment the query/pipeline ran.
    typeInfoCache :: !(Map Oid TypeInfo)
  }

data FieldParser a = FieldParser
  { fieldValueParser :: ColumnInfo -> Maybe LBS.ByteString -> Either String a,
    fieldFmt :: !Format,
    allowedPgTypes :: ColumnInfo -> Bool
  }
  deriving stock (Functor)

data RowParser a = RowParser
  { fullRowParser :: [ColumnInfo] -> Parsec.Parser a,
    -- | Returns the same colInfos with a boolean indicating if
    -- the expected types match for each colInfo.
    rowColumnsTypeCheck :: [ColumnInfo] -> [(ColumnInfo, Bool)],
    resultColumnsFmts :: ![Format]
  }
  deriving stock (Functor, Generic)

instance Applicative RowParser where
  pure v = RowParser (const $ pure v) (map (,True)) []

  -- TODO: We've changed `RowParser` quite a bit since this last proof.
  -- Prove it again. But try to prove that `(->) a b` is Applicative if `b` is, and that
  -- Product types of applicatives are applicatives, too?

  -- Here's informal proof this is a sound Applicative as per its laws!
  --   Identity

  --     pure id <*> v = v
  -- RowParser (pure id) (const True) 0 <*> RowParser v' check numcols =
  -- RowParser (pure id <*> v') check numcols =
  -- RowParser v' check numcols = v
  -- Identity is good!

  -- Composition

  --     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  -- Left side:
  -- RowParser (pure (.)) (const True) 0 <*> RowParser u' checkU numU <*> RowParser v' checkV numV <*> RowParser w' checkW numW =
  -- RowParser (pure (.) <*> u') checkU numU <*> v <*> w =
  -- RowParser (pure (.) <*> u' <*> v') (combination of checkU and checkV) (numU + numV) <*> w =
  -- RowParser (pure (.) <*> u' <*> v' <*> w') (combination of (combination of checkU and checkV) and checkW) (numU + numV + numW) =
  -- Right side:
  -- RowParser u' checkU numU <*> (RowParser (v' <*> w') (combination of checkV and checkW) (numV + numW)) =
  -- RowParser (u' <*> (v' <*> w')) (combination of checkU and (combination of checkV and checkW)) (numV + numW + numU)
  -- The sum of ints is clearly the same on both sides.
  -- The applicative Parsers are law-abinding and so Composition applies inside, meaning the parser bits are equal too.
  -- The combined predicate functions remain. But it's "clear" that the list splitting is associative, so
  -- it seems these functions are equal, too.
  -- So composition applies!

  -- Homomorphism

  --     pure f <*> pure x = pure (f x)
  -- RowParser (pure f) (const True) 0 <*> RowParser (pure x) (const True) 0 =
  -- RowParser (pure f <*> pure x) (const True) 0 =
  -- RowParser (f <$> pure x) (const True) 0 =
  -- RowParser (pure (f x)) (const True) 0 = pure (f x)
  -- Homomorphism is good!

  -- Interchange

  --     u <*> pure y = pure ($ y) <*> u
  -- Left side:
  -- RowParser u' check numcols <*> RowParser (pure y) (const True) 0 =
  -- RowParser (u' <*> pure y) check numcols
  -- Right side:
  -- RowParser (pure ($ y)) (const True) 0 <*> RowParser u' check numcols =
  -- RowParser (pure ($ y) <*> u') check numcols =
  -- Since inside RowParser we have an attoparsec Parser, which is a law-abiding Applicative,
  -- we can assume Interchange is valid for it, which means that:
  -- u' <*> pure y = pure ($ y) <*> u'
  -- And so left hand and right hand sides are equal, and Interchange applies!

  RowParser p1 tc1 nc1 <*> RowParser p2 tc2 nc2 = RowParser (\colTypes -> let (cols1, cols2) = List.splitAt (length nc1) colTypes in p1 cols1 <*> p2 cols2) (\colTypes -> let (cols1, cols2) = List.splitAt (length nc1) colTypes in tc1 cols1 ++ tc2 cols2) (nc1 ++ nc2)

-- TODO: Monad instance because people do want to write
-- <- (bind arrows) in their FromPgField instances..
-- and also because `fail` feels better than `error`?
-- instance Monad RowParser where
--   (>>=) = (<*>)

-- | TODO: I think this can actually be exported. Maybe it helps users avoid `Only` if they prefer?
singleColRowParser :: FieldParser a -> RowParser a
singleColRowParser (FieldParser {..}) =
  RowParser
    { fullRowParser = \case
        [singleColInfo] ->
          let decode = fieldValueParser singleColInfo
           in do
                lenNextCol <- fromIntegral <$> int32Parser
                nextColBs <-
                  if lenNextCol /= (-1)
                    then
                      if lenNextCol == 0
                        then pure $ Just ""
                        else Just . LBS.fromStrict <$> Parsec.take lenNextCol
                    else pure Nothing
                case decode nextColBs of
                  Right v -> pure v
                  Left err -> fail err
        _ -> error "singleColRowParser expected a single column OID but got 0 or >1",
      rowColumnsTypeCheck = \case
        [singleColInfo] -> [(singleColInfo, allowedPgTypes singleColInfo)]
        _ -> error "singleColRowParser's rowColumnsTypeCheck expected a single column OID but got 0 or >1",
      resultColumnsFmts = [fieldFmt]
    }

data AllowNull (a :: Bool) where
  AllowNull :: AllowNull True
  DisallowNull :: AllowNull False

type family ResAllowNull (r :: Bool) (a :: Type) :: Type where
  ResAllowNull True a = Maybe a
  ResAllowNull False a = a

compositeTypeParser :: forall a t. AllowNull t -> RowParser a -> FieldParser (ResAllowNull t a)
compositeTypeParser nullCheck (RowParser {..}) =
  case nullCheck of
    DisallowNull ->
      FieldParser
        { fieldValueParser = \compositeTypeOid -> \case
            Nothing -> Left "Got NULL in composite type but it was not allowed"
            Just bs -> Parsec.parseOnly (parserForRecord compositeTypeOid.typeInfoCache <* Parsec.endOfInput) bs,
          allowedPgTypes = const True, -- There's no way to enforce a custom type's OID. We only check if it's structurally the same in the parser (same subtypes in same order)
          fieldFmt = BinaryFmt -- TODO: What if the supplied RowParser has mixed Text and Binary formats?
        }
    AllowNull ->
      FieldParser
        { fieldValueParser = \compositeTypeColInfo -> \case
            Nothing -> Right Nothing
            Just bs -> Just <$> Parsec.parseOnly (parserForRecord compositeTypeColInfo.typeInfoCache <* Parsec.endOfInput) bs,
          allowedPgTypes = const True, -- There's no way to enforce a custom type's OID. We only check if it's structurally the same in the parser (same subtypes in same order)
          fieldFmt = BinaryFmt -- TODO: What if the supplied RowParser has mixed Text and Binary formats?
        }
  where
    parserForRecord :: Map Oid TypeInfo -> Parsec.Parser a
    parserForRecord typeInfoCache = do
      -- From https://github.com/postgres/postgres/blob/50ba65e73325cf55fedb3e1f14673d816726923b/src/backend/utils/adt/rowtypes.c#L687
      -- we can see a composite type's binary representation consists of: number of columns (Int32) + for_each_column { OID (Int32) + size_or_minus_1 (Int32) + Bytes }
      numCols <- fromIntegral <$> int32Parser
      let numColsExpected = length resultColumnsFmts
      unless (numCols == numColsExpected) $ fail $ "Composite type has " ++ show numCols ++ " attributes but parser expected " ++ show numColsExpected
      let mkColInfo = \oid -> ColumnInfo oid typeInfoCache
      cols <- replicateM numCols $ do
        !oid <- Oid . fromIntegral <$> int32Parser
        (LBS.fromStrict -> sizeBs, !size) <- Parsec.match $ fromIntegral <$> int32Parser
        !bs <- LBS.fromStrict <$> Parsec.take (max 0 size)
        pure (oid, sizeBs <> bs)
      let typecheckedCols = rowColumnsTypeCheck (map (mkColInfo . fst) cols)
      unless (all snd typecheckedCols) $ fail $ "Parser for composite found type OIDs " ++ show (map fst cols) ++ " but expected different"
      case Parsec.parseOnly (fullRowParser (map (mkColInfo . fst) cols) <* Parsec.endOfInput) (mconcat $ map snd cols) of
        Right v -> pure v
        Left err -> error $ "Error decoding composite type: " ++ show err

instance (FromPgField a) => FromPgRow (Only a) where
  rowParser = Only <$> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b) => FromPgRow (a, b) where
  rowParser = (,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c) => FromPgRow (a, b, c) where
  rowParser = (,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d) => FromPgRow (a, b, c, d) where
  rowParser = (,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e) => FromPgRow (a, b, c, d, e) where
  rowParser = (,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f) => FromPgRow (a, b, c, d, e, f) where
  rowParser = (,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g) => FromPgRow (a, b, c, d, e, f, g) where
  rowParser = (,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h) => FromPgRow (a, b, c, d, e, f, g, h) where
  rowParser = (,,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i) => FromPgRow (a, b, c, d, e, f, g, h, i) where
  rowParser = (,,,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j) => FromPgRow (a, b, c, d, e, f, g, h, i, j) where
  rowParser = (,,,,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j, FromPgField k) => FromPgRow (a, b, c, d, e, f, g, h, i, j, k) where
  rowParser = (,,,,,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j, FromPgField k, FromPgField l) => FromPgRow (a, b, c, d, e, f, g, h, i, j, k, l) where
  rowParser = (,,,,,,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

instance (FromPgField a, FromPgField b, FromPgField c, FromPgField d, FromPgField e, FromPgField f, FromPgField g, FromPgField h, FromPgField i, FromPgField j, FromPgField k, FromPgField l, FromPgField m) => FromPgRow (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  rowParser = (,,,,,,,,,,,,) <$> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser <*> singleColRowParser fieldParser

class FromPgField a where
  fieldParser :: FieldParser a

class FromPgRow a where
  rowParser :: RowParser a
  default rowParser :: (Generic a, ProductTypeDecoder (Rep a)) => RowParser a
  rowParser = genericFromPgRow

class ToPgField a where
  -- When writing a ToPgField MyCustomType instance, users can't know
  -- the OID of their custom types.
  -- Some options are available:
  -- 1. Make `toTypeOid` return a `Maybe Oid` for those cases.
  -- 2. Make `toTypeOid` return `Either TypeName Oid`.
  -- With 1, users might have to add `::mycustomtype` assertions to their
  -- queries occasionally, because postgres doesn't always infer types correctly.
  -- With 2, hpgsql could run a query first to get the type's OIDs, then the user's query.
  -- 1 seems much simpler and familiar.
  toTypeOid :: proxy a -> Maybe Oid
  toTypeOid _ = Nothing -- Default definition so users don't have to write this
  toPgField :: a -> Maybe LBS.ByteString

instance ToPgField Int where
  toTypeOid _ = Just haskellIntOid
  toPgField n = Just $ binaryIntEncoder n

instance ToPgField Int16 where
  toTypeOid _ = Just int2Oid
  toPgField n = Just $ Cereal.encodeLazy @Int16 . fromIntegral $ n

instance ToPgField Int32 where
  toTypeOid _ = Just int4Oid
  toPgField n = Just $ Cereal.encodeLazy @Int32 . fromIntegral $ n

instance ToPgField Int64 where
  toTypeOid _ = Just int8Oid
  toPgField n = Just $ Cereal.encodeLazy @Int64 . fromIntegral $ n

instance ToPgField Integer where
  toTypeOid _ = Just numericOid
  toPgField n = toPgField @Scientific (fromIntegral n)

instance ToPgField Oid where
  toTypeOid _ = Just oidOid
  toPgField n = Just $ Cereal.encodeLazy @Int32 . fromIntegral $ n

instance ToPgField Scientific where
  toTypeOid _ = Just numericOid
  toPgField n =
    let sign = Cereal.encodeLazy @Int16 $ if n >= 0 then 0 else 0x4000
        -- The number is coeff * 10^exp, but we want it in base-10000 so we convert it to
        -- new_coeff * 10^new_exp with new_exp a multiple of 4
        base10000Expon = 4 * (base10Exponent n `div` 4)
        base10000Coeff = coefficient n * expt 10 (base10Exponent n - base10000Expon)
        ndigits, weight :: Int16
        digits :: LBS.LazyByteString
        (ndigits, weight, digits) = calculateDigits 0 0 (abs base10000Coeff) ""
        dscale = Cereal.encodeLazy @Int16 (abs $ fromIntegral base10000Expon) -- More than necessary, but safe?
     in Just $ Cereal.encodeLazy ndigits <> Cereal.encodeLazy (weight - 1 + fromIntegral (base10000Expon `div` 4)) <> sign <> dscale <> digits
    where
      calculateDigits :: Int16 -> Int16 -> Integer -> LBS.LazyByteString -> (Int16, Int16, LBS.LazyByteString)
      calculateDigits !ndigitsSoFar !weightSoFar 0 !encodedDigits = (ndigitsSoFar, weightSoFar, encodedDigits)
      calculateDigits !ndigitsSoFar !weightSoFar !val !encodedDigits =
        let (quotient, fromIntegral -> rest :: Int16) = val `divMod` 10000
         in calculateDigits
              (ndigitsSoFar + 1)
              (weightSoFar + 1)
              quotient
              (Cereal.encodeLazy @Int16 rest <> encodedDigits)

instance ToPgField Float where
  toTypeOid _ = Just float4Oid
  toPgField n = Just $ Cereal.encodeLazy @Word32 $ castFloatToWord32 n

instance ToPgField Double where
  toTypeOid _ = Just float8Oid
  toPgField n = Just $ Cereal.encodeLazy @Word64 $ castDoubleToWord64 n

instance ToPgField Bool where
  -- TODO: Cereal.encodeLazy seems to work, but reference the documentation that shows how bools are encoded
  toTypeOid _ = Just boolOid
  toPgField n = Just $ Cereal.encodeLazy @Bool $ n

instance ToPgField Day where
  -- PG Dates are Int32 number of days relative to 2000-01-01
  -- https://github.com/postgres/postgres/blob/master/src/include/datatype/timestamp.h#L235
  toTypeOid _ = Just dateOid
  toPgField d = Just $ Cereal.encodeLazy @Int32 $ daysSince2000
    where
      -- TODO: Catch integer overflow and do what?
      daysSince2000 = fromIntegral $ diffDays d (fromGregorian 2000 1 1)

instance ToPgField CalendarDiffTime where
  toTypeOid _ = Just intervalOid
  toPgField CalendarDiffTime {..} =
    let (days :: Int32, timeUnderOneDay) = ctTime `divMod'` 86_400_000_000_000_000
     in Just $ Cereal.encodeLazy @(Int64, Int32, Int32) (round $ timeUnderOneDay * 1_000_000, days, fromIntegral ctMonths)

instance ToPgField UTCTime where
  toTypeOid _ = Just timestamptzOid
  toPgField (UTCTime parsedDate timeinday) = Just $ Cereal.encodeLazy @Int64 totalusecs
    where
      -- TODO: Catch integer overflow and do what?
      -- This is FromPgField UTCTime:
      -- let totalusecs = Binary.decode @Int64 $ BS.fromStrict bs
      --     (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
      --     parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day - 13)) $ fromJulian 2000 01 01
      --  in Right $ UTCTime parsedDate (realToFrac $ realToFrac @Int64 @Double timeusecs / 1_000_000)
      -- Which better translates to:
      --     totalusecs = day*USECS_PER_DAY + timeusecs
      --     day = parsedDate - 2000-01-01 + 13days
      -- day :: Int64 = 13 + (fromInteger $ parsedDate `diffDays` fromJulian 2000 01 01)
      day :: Int64 = fromInteger $ parsedDate `diffDays` fromJulian 1999 12 19
      totalusecs :: Int64 = 86_400_000_000 * day + fromInteger (diffTimeToPicoseconds timeinday `div` 1_000_000)

instance ToPgField ZonedTime where
  toTypeOid _ = Just timestamptzOid
  toPgField = toPgField @UTCTime . zonedTimeToUTC

instance ToPgField Char where
  toTypeOid _ = Just textOid
  toPgField t = toPgField $ Text.singleton t

instance ToPgField ByteString where
  toTypeOid _ = Just byteaOid
  toPgField = Just . LBS.fromStrict

instance ToPgField LBS.ByteString where
  toTypeOid _ = Just byteaOid
  toPgField = Just

instance ToPgField Text where
  toTypeOid _ = Just textOid

  -- TODO: What about client_encoding?
  -- TODO: Some unsafe Text->LazyByteString conversion function that is faster?
  toPgField t = Just $ LBS.fromStrict $ encodeUtf8 t

instance ToPgField LT.Text where
  toTypeOid _ = Just textOid

  -- TODO: What about client_encoding?
  -- TODO: Some unsafe LT.Text->LazyByteString conversion function that is faster?
  toPgField t = Just $ LT.encodeUtf8 t

instance ToPgField String where
  toTypeOid _ = Just textOid

  -- TODO: What about client_encoding?
  toPgField = toPgField . Text.pack

instance ToPgField Aeson.Value where
  -- Maybe we shouldn't specify an oid so postgres can infer the best type?
  -- But json and jsonb might have different binary representations..
  toTypeOid _ = Just jsonbOid
  toPgField v = Just $ LBS.cons 1 (Aeson.encode v)

instance (ToPgField a) => ToPgField (Maybe a) where
  toTypeOid _ = toTypeOid (Proxy @a)
  toPgField Nothing = Nothing
  toPgField (Just n) = toPgField n

class ToPgRow a where
  toPgParams :: a -> [(Maybe Oid, Maybe LBS.ByteString)]

instance ToPgRow () where
  toPgParams () = []

instance (ToPgField a) => ToPgRow (Only a) where
  toPgParams (Only a) = [(toTypeOid (Proxy @a), toPgField a)]

instance (ToPgField a, ToPgField b) => ToPgRow (a, b) where
  toPgParams (a, b) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b)]

instance (ToPgField a, ToPgField b, ToPgField c) => ToPgRow (a, b, c) where
  toPgParams (a, b, c) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d) => ToPgRow (a, b, c, d) where
  toPgParams (a, b, c, d) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e) => ToPgRow (a, b, c, d, e) where
  toPgParams (a, b, c, d, e) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f) => ToPgRow (a, b, c, d, e, f) where
  toPgParams (a, b, c, d, e, f) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e), (toTypeOid (Proxy @f), toPgField f)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g) => ToPgRow (a, b, c, d, e, f, g) where
  toPgParams (a, b, c, d, e, f, g) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e), (toTypeOid (Proxy @f), toPgField f), (toTypeOid (Proxy @g), toPgField g)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h) => ToPgRow (a, b, c, d, e, f, g, h) where
  toPgParams (a, b, c, d, e, f, g, h) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e), (toTypeOid (Proxy @f), toPgField f), (toTypeOid (Proxy @g), toPgField g), (toTypeOid (Proxy @h), toPgField h)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i) => ToPgRow (a, b, c, d, e, f, g, h, i) where
  toPgParams (a, b, c, d, e, f, g, h, i) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e), (toTypeOid (Proxy @f), toPgField f), (toTypeOid (Proxy @g), toPgField g), (toTypeOid (Proxy @h), toPgField h), (toTypeOid (Proxy @i), toPgField i)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i, ToPgField j) => ToPgRow (a, b, c, d, e, f, g, h, i, j) where
  toPgParams (a, b, c, d, e, f, g, h, i, j) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e), (toTypeOid (Proxy @f), toPgField f), (toTypeOid (Proxy @g), toPgField g), (toTypeOid (Proxy @h), toPgField h), (toTypeOid (Proxy @i), toPgField i), (toTypeOid (Proxy @j), toPgField j)]

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i, ToPgField j, ToPgField k) => ToPgRow (a, b, c, d, e, f, g, h, i, j, k) where
  toPgParams (a, b, c, d, e, f, g, h, i, j, k) = [(toTypeOid (Proxy @a), toPgField a), (toTypeOid (Proxy @b), toPgField b), (toTypeOid (Proxy @c), toPgField c), (toTypeOid (Proxy @d), toPgField d), (toTypeOid (Proxy @e), toPgField e), (toTypeOid (Proxy @f), toPgField f), (toTypeOid (Proxy @g), toPgField g), (toTypeOid (Proxy @h), toPgField h), (toTypeOid (Proxy @i), toPgField i), (toTypeOid (Proxy @j), toPgField j), (toTypeOid (Proxy @k), toPgField k)]

instance (ToPgField a) => ToPgRow [a] where
  toPgParams colValues = let typOid = toTypeOid (Proxy @a) in map (\v -> (typOid, toPgField v)) colValues

-- | The OID for `Data.Int`, which is machine dependent.
haskellIntOid :: Oid

-- | All pg type OIDs that fit into Haskell's `Data.Int`, whose size is machine dependent.
haskellIntOids :: [Oid]
(haskellIntOid, haskellIntOids)
  | (fromIntegral (maxBound @Int) :: Integer) > fromIntegral (maxBound @Int32) = (int8Oid, [int2Oid, int4Oid, int8Oid])
  | (fromIntegral (maxBound @Int) :: Integer) > fromIntegral (maxBound @Int16) = (int4Oid, [int2Oid, int4Oid])
  | otherwise = (int2Oid, [int2Oid])

-- | Big-Endian binary encoder for Haskell's `Data.Int`, which is machine-dependent.
binaryIntEncoder :: Int -> LBS.ByteString
binaryIntEncoder
  | haskellIntOid == int8Oid = Cereal.encodeLazy @Int64 . fromIntegral
  | haskellIntOid == int4Oid = Cereal.encodeLazy @Int32 . fromIntegral
  | otherwise = Cereal.encodeLazy @Int16 . fromIntegral

-- | Big-Endian binary decoder for Haskell's various IntXX types.
binaryIntDecoder :: forall a. (Integral a, Bounded a) => Oid -> LBS.ByteString -> Either String a
binaryIntDecoder typOid = \bs ->
  if doesFit
    then intDecoder bs
    else Left $ "Chosen integral type does not fit every value for PG type with OID " ++ show typOid
  where
    maxBoundPgType :: Integer
    intDecoder :: LBS.ByteString -> Either String a
    (maxBoundPgType, intDecoder)
      | typOid == int8Oid = (fromIntegral $ maxBound @Int64, fmap fromIntegral . Cereal.decodeLazy @Int64)
      | typOid == int4Oid = (fromIntegral $ maxBound @Int32, fmap fromIntegral . Cereal.decodeLazy @Int32)
      | typOid == int2Oid = (fromIntegral $ maxBound @Int16, fmap fromIntegral . Cereal.decodeLazy @Int16)
      | otherwise = error "Bug in HPgsql. Decoding binary integral type not an int2, int4 or int8"
    doesFit = maxBoundPgType <= fromIntegral (maxBound @a)

binaryFloat4Decoder :: LBS.ByteString -> Float
binaryFloat4Decoder = castWord32ToFloat . either error id . Cereal.decodeLazy @Word32

binaryFloat8Decoder :: LBS.ByteString -> Double
binaryFloat8Decoder = castWord64ToDouble . either error id . Cereal.decodeLazy @Word64

parsePgType :: Format -> [Oid] -> (Maybe LBS.ByteString -> Either String a) -> FieldParser a
parsePgType fieldFmt requiredTypeOids fieldValueParser =
  FieldParser
    { fieldValueParser = \_oid -> fieldValueParser,
      fieldFmt,
      allowedPgTypes = (`elem` requiredTypeOids) . typeOid
    }

instance FromPgField () where
  fieldParser =
    FieldParser
      { fieldValueParser = \_oid -> \case
          Just "" -> Right ()
          Just bs -> Left $ "Invalid value '" ++ show bs ++ "' for postgres void type"
          Nothing -> Left "Cannot decode SQL null as the Haskell () type. Use a `Maybe ()`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (== voidOid) . typeOid
      }

instance FromPgField Int where
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int type. Use a `Maybe Int`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` haskellIntOids) . typeOid
      }

instance FromPgField Int16 where
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int16 type. Use a `Maybe Int16`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (== int2Oid) . typeOid
      }

instance FromPgField Int32 where
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int32 type. Use a `Maybe Int32`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [int2Oid, int4Oid]) . typeOid
      }

instance FromPgField Int64 where
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decode = binaryIntDecoder oid
           in \case
                Just bs -> decode bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Int64 type. Use a `Maybe Int64`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [int2Oid, int4Oid, int8Oid]) . typeOid
      }

instance FromPgField Integer where
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decodeInt = binaryIntDecoder @Int64 oid
           in \case
                Just bs
                  | oid /= numericOid -> fromIntegral <$> decodeInt bs
                  | otherwise -> case Parsec.parseOnly (scientificDecoder True <* Parsec.endOfInput) bs of
                      Right sci -> case floatingOrInteger @Double @Integer sci of
                        Right i -> Right i
                        Left _ -> Left "Internal error in Hpgsql. Scientific to Integer conversion failed"
                      Left err -> Left err
                Nothing -> Left "Cannot decode SQL null as the Haskell Integer type. Use a `Maybe Integer`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [int8Oid, numericOid, int4Oid, int2Oid]) . typeOid
      }

instance FromPgField Oid where
  fieldParser =
    FieldParser
      { fieldValueParser = \_ -> \case
          -- Oids are just int4
          Just bs -> Oid <$> binaryIntDecoder int4Oid bs
          Nothing -> Left "Cannot decode SQL null as the Haskell Oid type. Use a `Maybe Oid`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (== oidOid) . typeOid
      }

instance FromPgField Float where
  fieldParser = parsePgType BinaryFmt [float4Oid] $ \case
    Just bs -> Right $ binaryFloat4Decoder bs
    Nothing -> Left "Cannot decode SQL null as the Haskell Float type. Use a `Maybe Float`"

instance FromPgField Double where
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decoder
                | oid == float8Oid = binaryFloat8Decoder
                | otherwise = float2Double . binaryFloat4Decoder
           in \case
                Just bs -> Right $ decoder bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Double type. Use a `Maybe Double`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [float8Oid, float4Oid]) . typeOid
      }

-- | A parser that accepts any PG type and returns the object's
-- postgres' binary representation as a ByteString.
-- This can be useful to build `FromPgField` instances for enum types,
-- since postgres uses their UTF8 text representation even in the
-- binary protocol, but otherwise it's easier to compose existing
-- FieldParsers.
anyTypeDecoder :: FieldParser LBS.ByteString
anyTypeDecoder =
  FieldParser
    { fieldValueParser = \_oid -> \case
        Nothing -> Left "Cannot decode SQL null as the `anyTypeDecoder`."
        Just bs -> Right bs,
      fieldFmt = BinaryFmt,
      allowedPgTypes = const True
    }

scientificDecoder :: Bool -> Parsec.Parser Scientific
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
    parseAndMult :: Int16 -> Int -> Scientific -> Parsec.Parser Scientific
    parseAndMult 0 _ !val = pure val
    parseAndMult !ndigitsLeft !currexpon !val = do
      !digit <- fromIntegral <$> int16Parser
      parseAndMult (ndigitsLeft - 1) (currexpon - 4) (val + scientific digit currexpon)

instance FromPgField Scientific where
  -- See https://github.com/postgres/postgres/blob/799959dc7cf0e2462601bea8d07b6edec3fa0c4f/src/backend/utils/adt/numeric.c#L1163
  fieldParser =
    FieldParser
      { fieldValueParser = \ColumnInfo {typeOid = oid} ->
          let !decodeInt = binaryIntDecoder @Int64 oid
           in \case
                Just bs ->
                  -- TODO: There is loss converting from Float/Double to Scientific, but it might be quite small, so should we accept
                  -- float4Oid and float8Oid here?
                  if oid == numericOid
                    then Parsec.parseOnly (scientificDecoder False <* Parsec.endOfInput) bs
                    else flip scientific 0 . fromIntegral <$> decodeInt bs
                Nothing -> Left "Cannot decode SQL null as the Haskell Scientific type. Use a `Maybe Scientific`",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [numericOid, int2Oid, int4Oid, int8Oid]) . typeOid
      }

binaryTrue :: LBS.ByteString
binaryTrue = Cereal.encodeLazy True

instance FromPgField Bool where
  fieldParser = parsePgType BinaryFmt [boolOid] $ \case
    Just bs -> Right $ bs == binaryTrue
    Nothing -> Left "Cannot decode SQL null as the Haskell Bool type. Use a `Maybe Bool`"

instance FromPgField Char where
  fieldParser =
    let textParser = fieldValueParser (fieldParser @Text)
     in FieldParser
          { fieldValueParser = \colInfo@ColumnInfo {typeOid = oid} ->
              let !decodeText = textParser colInfo
               in \mbs -> case mbs of
                    Just bs ->
                      if oid == charOid
                        -- TODO: Postgres has values of type "char" in the pg_type.typcategory table.
                        -- We should test this instance works with those, and we haven't yet.
                        then Right $ Data.ByteString.Lazy.Char8.head bs
                        else case decodeText mbs of
                          Left err -> Left err
                          Right t -> if Text.length t > 1 then Left "Cannot parse text with more than one character into a Haskell Char type." else Right (Text.head t)
                    Nothing -> Left "Cannot decode SQL null as the Haskell Char type. Use a `Maybe Char`",
            fieldFmt = BinaryFmt,
            -- TODO: All the varchar types?
            allowedPgTypes = (`elem` [charOid, textOid]) . typeOid
          }

instance FromPgField ByteString where
  fieldParser = parsePgType BinaryFmt [byteaOid] $ \case
    Just bs -> Right $ LBS.toStrict bs
    Nothing -> Left "Cannot decode SQL null as the Haskell ByteString type. Use a `Maybe ByteString`"

instance FromPgField LBS.ByteString where
  fieldParser = parsePgType BinaryFmt [byteaOid] $ \case
    Just bs -> Right bs
    Nothing -> Left "Cannot decode SQL null as the Haskell ByteString type. Use a `Maybe ByteString`"

instance FromPgField Text where
  fieldParser = parsePgType BinaryFmt [textOid, varcharOid, nameOid] $ \case
    Just bs -> Right $ decodeUtf8 $ LBS.toStrict bs -- TODO: Ensure we set client_encoding=utf8 in our connections!
    -- TODO: Use some faster unsafeDecodeUtf8 function?
    Nothing -> Left "Cannot decode SQL null as the Haskell Text type. Use a `Maybe Text`"

instance FromPgField LT.Text where
  fieldParser = parsePgType BinaryFmt [textOid, varcharOid, nameOid] $ \case
    Just bs -> Right $ LT.decodeUtf8 bs -- TODO: Ensure we set client_encoding=utf8 in our connections!
    -- TODO: Use some faster unsafeDecodeUtf8 function?
    Nothing -> Left "Cannot decode SQL null as the Haskell Text type. Use a `Maybe Text`"

instance FromPgField String where
  fieldParser = parsePgType BinaryFmt [textOid, varcharOid, nameOid] $ \case
    Just bs -> Right $ Text.unpack $ decodeUtf8 $ LBS.toStrict bs -- TODO: Ensure we set client_encoding=utf8 in our connections!
    -- TODO: Use some faster unsafeDecodeUtf8 function?
    Nothing -> Left "Cannot decode SQL null as the Haskell String type. Use a `Maybe String`"

instance FromPgField UTCTime where
  fieldParser = parsePgType BinaryFmt [timestamptzOid] $ \case
    Just bs -> do
      -- See https://github.com/postgres/postgres/blob/50cb7505b3010736b9a7922e903931534785f3aa/src/backend/utils/adt/timestamp.c#L1909
      totalusecs <- Cereal.decodeLazy @Int64 bs
      let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
          parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
      Right $ UTCTime parsedDate (picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell UTCTime type. Use a `Maybe UTCTime`"

instance FromPgField ZonedTime where
  fieldParser = parsePgType BinaryFmt [timestamptzOid] $ \case
    Just bs -> do
      -- See https://github.com/postgres/postgres/blob/50cb7505b3010736b9a7922e903931534785f3aa/src/backend/utils/adt/timestamp.c#L1909
      totalusecs <- Cereal.decodeLazy @Int64 bs
      let (day, timeusecs) = totalusecs `divMod` 86_400_000_000 -- USECS per day
          parsedDate = addJulianDurationClip (CalendarDiffDays 0 (fromIntegral day)) $ fromJulian 1999 12 19
      Right $ utcToZonedTime utc $ UTCTime parsedDate (picosecondsToDiffTime $ fromIntegral timeusecs * 1_000_000)
    Nothing -> Left "Cannot decode SQL null as the Haskell ZonedTime type. Use a `Maybe ZonedTime`"

instance FromPgField Day where
  fieldParser = parsePgType BinaryFmt [dateOid] $ \case
    Just bs -> do
      -- There is a very specific conversion function for these, which I poorly translated to Haskell
      -- https://github.com/postgres/postgres/blob/799959dc7cf0e2462601bea8d07b6edec3fa0c4f/src/backend/utils/adt/datetime.c#L321
      -- But I found a simpler way to do this. Let's see if it works in our property based tests
      jd <- Cereal.decodeLazy @Int32 bs
      Right $ addJulianDurationClip (CalendarDiffDays 0 (fromIntegral jd - 13)) $ fromJulian 2000 01 01
    Nothing -> Left "Cannot decode SQL null as the Haskell Day type. Use a `Maybe Day`"

instance FromPgField CalendarDiffTime where
  fieldParser = parsePgType BinaryFmt [intervalOid] $ \case
    Just bs -> do
      (nMicrosecs :: Int64, nDays :: Int32, nMonths :: Int32) <- Cereal.decodeLazy bs
      Right $ CalendarDiffTime {ctMonths = fromIntegral nMonths, ctTime = secondsToNominalDiffTime (fromIntegral nDays * 86400) + realToFrac (picosecondsToDiffTime (fromIntegral nMicrosecs * 1_000_000))}
    Nothing -> Left "Cannot decode SQL null as the Haskell CalendarDiffTime type. Use a `Maybe CalendarDiffTime`"

instance FromPgField Aeson.Value where
  fieldParser =
    FieldParser
      { fieldValueParser =
          \ColumnInfo {typeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if typeOid == jsonbOid then LBS.drop 1 else Prelude.id
             in \mbs ->
                  case mbs of
                    Just bs -> case Aeson.decode $ fixJsonb bs of
                      Just d -> Right d
                      Nothing -> Left "Bug in HPgsql. Postgres produced a json or jsonb value that Aeson does not consider valid."
                    Nothing -> Left "Cannot decode SQL null as the Haskell Aeson.Value type. Use a `Maybe Aeson.Value` if you want SQL nulls",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . typeOid
      }

instance (FromPgField a) => FromPgField (Maybe a) where
  fieldParser =
    let (!FieldParser {..}) = fieldParser
     in FieldParser
          { fieldValueParser = \oid ->
              let !origFieldValueParser = fieldValueParser oid
               in \case
                    Nothing -> Right Nothing
                    justBs -> Just <$> origFieldValueParser justBs,
            fieldFmt,
            allowedPgTypes
          }

instance forall a. (FromPgField a) => FromPgField (Vector a) where
  -- From https://github.com/postgres/postgres/blob/5941946d0934b9eccb0d5bfebd40b155249a0130/src/backend/utils/adt/arrayfuncs.c#L1548
  fieldParser =
    FieldParser
      { fieldValueParser = \colInfo ->
          let !arrayFieldParser = arrayParser colInfo.typeInfoCache <* Parsec.endOfInput
           in \case
                Nothing -> Left "Cannot decode SQL null as the Haskell Vector type. Use a `Maybe (Vector a)`"
                Just bs -> Parsec.parseOnly arrayFieldParser bs,
        fieldFmt = BinaryFmt,
        allowedPgTypes = const True -- TODO: We could put "Is-Array" in the typeinfo cache and reject when it's not an array here
      }
    where
      !elementParser = fieldParser @a
      arrayParser :: Map Oid TypeInfo -> Parsec.Parser (Vector a)
      arrayParser typeInfoCache = do
        !ndim <- int32Parser
        !_hasNull <- int32Parser
        !elementTypeOid :: Oid <- Oid . fromIntegral <$> int32Parser
        let !elementColInfo = ColumnInfo elementTypeOid typeInfoCache
        when (ndim /= 1) $ fail $ "TODO: No support for multi-dimensional arrays in HPgsql. Got array with ndim=" ++ show ndim
        !dim_i :: Int <- fromIntegral <$> int32Parser
        !_lb_i <- int32Parser
        -- TODO: Check binary/text compatibility somehow?
        -- TODO: Allocate vector with right size for performance, use `runST` or something?
        unless (elementParser.allowedPgTypes elementColInfo) $ fail $ "Array contains elements of type OID " ++ show elementTypeOid ++ " but decoder does not handle that type"
        fmap Vector.fromList $ replicateM dim_i $ do
          size :: Int <- fromIntegral <$> int32Parser
          elementBs <- if size == (-1) then pure Nothing else Just . LBS.fromStrict <$> Parsec.take size
          case elementParser.fieldValueParser elementColInfo elementBs of
            Left err -> fail $ "Error parsing array element: " ++ show err
            Right el -> pure el

int32Parser :: Parsec.Parser Int32
int32Parser = either fail pure . Cereal.decode @Int32 =<< Parsec.take 4

int16Parser :: Parsec.Parser Int16
int16Parser = either fail pure . Cereal.decode @Int16 =<< Parsec.take 2

-- {- Generic deriving -}
genericFromPgRow :: forall a. (Generic a, ProductTypeDecoder (Rep a)) => RowParser a
genericFromPgRow = to <$> genRowDecoder @(Rep a)

class ProductTypeDecoder f where
  genRowDecoder :: RowParser (f a)

instance (ProductTypeDecoder a, ProductTypeDecoder b) => ProductTypeDecoder (a :*: b) where
  genRowDecoder = (:*:) <$> genRowDecoder <*> genRowDecoder

instance (ProductTypeDecoder f) => ProductTypeDecoder (M1 a c f) where
  genRowDecoder = M1 <$> genRowDecoder

instance (FromPgField a) => ProductTypeDecoder (K1 r a) where
  genRowDecoder = fmap K1 $ singleColRowParser $ fieldParser @a

newtype LowerCasedPgEnum a = LowerCasedPgEnum a

-- | TODO: Why does this instance require UndecidableInstances?
instance (Generic a, EnumDecoder (Rep a)) => FromPgField (LowerCasedPgEnum a) where
  fieldParser = LowerCasedPgEnum <$> genericEnumFieldParser LT.toLower

genericEnumFieldParser ::
  forall a.
  (Generic a, EnumDecoder (Rep a)) =>
  -- | A function that takes in the Haskell constructor name and returns the textual representation of the enum in postgres
  (LT.Text -> LT.Text) ->
  FieldParser a
genericEnumFieldParser nameTransform = fromMaybe (error $ "Invalid enum value. Not one of " ++ show (Map.keys allValuesMap)) . flip Map.lookup allValuesMap <$> anyTypeDecoder
  where
    -- TODO: Vector of pointers to ByteStrings for a bit more memory locality? Does it make a perf difference?
    allValuesMap = Map.mapKeys (LT.encodeUtf8 . nameTransform) $ fmap to genEnumDecoder

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
