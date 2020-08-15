module HPgsql.Newtypes
  ( Aeson (..),
    PgJson,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonInternal
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Typeable)
import HPgsql.Field (FromPgField (..), ToPgField (..), parsePgType)
import HPgsql.TypeInfo (Format (..), jsonOid, jsonbOid)

-- TODO: Write tests for these types!

-- | A JSON type that does not incur the costs of deserializing
-- in its `FromPgField` instance because it assumes postgres only generates
-- valid JSON. Useful for extra performance if its opaqueness is not a problem.
-- Although it does have a `toJSON` method, using it will incur a
-- deserialization cost, so if you find yourself using that too much consider just using
-- `Aeson.Value` or the `Aeson` newtype instead of this.
newtype PgJson = PgJson LBS.ByteString

instance ToJSON PgJson where
  toJSON (PgJson bs) = case Aeson.eitherDecode' bs of
    Left err -> error $ "Bug in HPgsql. PgJson not valid JSON: " ++ err
    Right v -> v

  toEncoding (PgJson bs) = AesonInternal.unsafeToEncoding (Builder.lazyByteString bs)

instance FromPgField PgJson where
  fieldParser = parsePgType TextFmt [jsonOid, jsonbOid] $ \case
    Nothing -> Left "Cannot parse SQL null into Haskell PgJson type. Use a `Maybe PgJson` if you want SQL nulls"
    Just bs -> Right $ PgJson bs

-- | A newtype wrapper to decode a JSON value with Aeson
-- into your type (from either json or jsonb), and to encode
-- to jsonb.
newtype Aeson a = Aeson {getAeson :: a}
  deriving (Eq, Show, Read, Typeable, Functor)

instance (FromJSON a) => FromPgField (Aeson a) where
  fieldParser = parsePgType TextFmt [jsonOid, jsonbOid] $ \case
    Nothing -> Left "Cannot parse SQL null into Haskell (Aeson a) type. Use a `Maybe (Aeson a)` if you want SQL nulls"
    Just bs -> case Aeson.decode bs of
      Just v -> Right $ Aeson v
      Nothing -> Left "Failed to decode postgres JSON value into your `Aeson a` type"

instance (ToJSON a) => ToPgField (Aeson a) where
  -- \| Explicitly don't choose json or jsonb to avoid type checking errors
  -- in postgres.
  toTypeOid _ = Nothing
  toPgField (Aeson a) = Just $ Aeson.encode a
