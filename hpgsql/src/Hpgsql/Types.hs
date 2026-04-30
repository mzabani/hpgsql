module Hpgsql.Types
  ( Aeson (..),
    PgJson, -- Do not export ctor
    PGArray (..),
    pgJsonByteString,
  )
where

import Control.Monad (replicateM)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonInternal
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Typeable)
import Hpgsql.Builder (BinaryField (..))
import Hpgsql.Encoding (FieldInfo (..), FieldDecoder (..), FieldEncoder (..), FromPgField (..), ToPgField (..), arrayField, toPgVectorField)
import Hpgsql.TypeInfo (EncodingContext (..), TypeInfo (..), jsonOid, jsonbOid, lookupTypeByOid)

-- | Encodes a Haskell list as a postgres array. You can also use `Vector` if you prefer.
-- The reason for this type instead of allowing @[a]@ to be a field is that an instance
-- for @[a]@ would require an overlappable instance for @String@, and that is not ideal.
newtype PGArray a = PGArray {fromPGArray :: [a]}
  deriving (Eq, Ord, Read, Show, Functor)

instance forall a. (ToPgField a) => ToPgField (PGArray a) where
  fieldEncoder =
    let fe = fieldEncoder @a
     in FieldEncoder
          { toTypeOid = \encodingContext -> do
              elOid <- fe.toTypeOid encodingContext
              arrayTypInfo <- lookupTypeByOid elOid encodingContext.typeInfoCache
              arrayTypInfo.oidOfArrayType,
            toPgField = \encCtx -> toPgVectorField encCtx . fromPGArray
          }

instance forall a. (FromPgField a) => FromPgField (PGArray a) where
  fieldDecoder = PGArray <$> arrayField replicateM fieldDecoder

-- | A JSON type that does not incur the costs of deserializing
-- in its `FromPgField` instance because it assumes postgres only generates
-- valid JSON. Useful for extra performance if its opaqueness is not a problem.
-- Although it does have a `toJSON` method, using it will incur a
-- deserialization cost, so if you find yourself using that too much consider just using
-- `Aeson.Value` or the `Aeson` newtype instead of this.
newtype PgJson = PgJson ByteString

instance ToJSON PgJson where
  toJSON (PgJson bs) = case Aeson.eitherDecodeStrict' bs of
    Left err -> error $ "Bug in Hpgsql. PgJson not valid JSON: " ++ err
    Right v -> v

  toEncoding (PgJson bs) = AesonInternal.unsafeToEncoding (Builder.byteString bs)

-- | A valid UTF8 representation of the JSON value.
pgJsonByteString :: PgJson -> ByteString
pgJsonByteString (PgJson bs) = bs

instance FromPgField PgJson where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder =
          \FieldInfo {fieldTypeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if fieldTypeOid == jsonbOid then BS.drop 1 else Prelude.id
             in \case
                  Just bs -> Right $ PgJson $ fixJsonb bs
                  Nothing -> Left "Cannot decode SQL null as the Haskell PgJson type. Use a `Maybe PgJson` if you want SQL nulls",
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . fieldTypeOid
      }

-- | A newtype wrapper to decode a JSON value with Aeson
-- into your type (from either json or jsonb), and to encode
-- to jsonb.
newtype Aeson a = Aeson {getAeson :: a}
  deriving (Eq, Show, Read, Typeable, Functor)

instance (FromJSON a) => FromPgField (Aeson a) where
  fieldDecoder =
    FieldDecoder
      { fieldValueDecoder =
          \FieldInfo {fieldTypeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if fieldTypeOid == jsonbOid then BS.drop 1 else Prelude.id
             in \case
                  Just bs -> case Aeson.decodeStrict $ fixJsonb bs of
                    Just v -> Right $ Aeson v
                    Nothing -> Left "Failed to decode postgres JSON value into your `Aeson a` type. Are you sure it's proper JSON?"
                  Nothing -> Left "Cannot decode SQL null as a Haskell (Aeson a) type. Use a `Maybe (Aeson a)` if you want SQL nulls",
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . fieldTypeOid
      }

instance (ToJSON a) => ToPgField (Aeson a) where
  fieldEncoder =
    FieldEncoder
      { toTypeOid = \_ -> Just jsonbOid,
        toPgField = \_ (Aeson v) ->
          NotNull $ BS.cons 1 (LBS.toStrict $ Aeson.encode v)
      }
