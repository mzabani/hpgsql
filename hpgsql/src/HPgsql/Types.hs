module HPgsql.Types
  ( Aeson (..),
    PgJson, -- Do not export ctor
    Values (..),
    PGArray (..),
    valuesToQuery,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonInternal
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Proxy (..), Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import HPgsql.Builder (BinaryField (..))
import HPgsql.Encoding (ColumnInfo (..), FieldParser (..), FromPgField (..), ToPgField (..), ToPgRow (..))
import HPgsql.Query (Query (..), commaSeparatedRowTuples)
import HPgsql.TypeInfo (jsonOid, jsonbOid)

-- | Encodes a Haskell list as a postgres array. You can also use `Vector` if you prefer.
newtype PGArray a = PGArray {fromPGArray :: [a]}
  deriving (Eq, Ord, Read, Show, Functor)

instance forall a. (ToPgField a) => ToPgField (PGArray a) where
  toTypeOid _ = toTypeOid (Proxy @(Vector a))
  toPgField encCtx = toPgField encCtx . Vector.fromList . fromPGArray

instance forall a. (FromPgField a) => FromPgField (PGArray a) where
  fieldParser = PGArray . Vector.toList <$> fieldParser

newtype Values a = Values [a]

-- | Generates a query like @VALUES ($1,$2), ($3,$4)@ from a list of rows.
-- Can be embedded inside a @[sql|...|]@ quasiquote using @^{expr}@ syntax:
--
-- > [sql| INSERT INTO emp(id,name) ^{valuesToQuery (Values rows)} ON CONFLICT DO NOTHING |]
valuesToQuery :: (ToPgRow a) => Values a -> Query
valuesToQuery (Values []) = error "HPgsql: empty Values lists are not supported because postgres does not support them"
valuesToQuery (Values rows) =
  let allParams = map toPgParams rows
   in "VALUES " <> commaSeparatedRowTuples allParams

-- | A JSON type that does not incur the costs of deserializing
-- in its `FromPgField` instance because it assumes postgres only generates
-- valid JSON. Useful for extra performance if its opaqueness is not a problem.
-- Although it does have a `toJSON` method, using it will incur a
-- deserialization cost, so if you find yourself using that too much consider just using
-- `Aeson.Value` or the `Aeson` newtype instead of this.
newtype PgJson = PgJson ByteString

instance ToJSON PgJson where
  toJSON (PgJson bs) = case Aeson.eitherDecodeStrict' bs of
    Left err -> error $ "Bug in HPgsql. PgJson not valid JSON: " ++ err
    Right v -> v

  toEncoding (PgJson bs) = AesonInternal.unsafeToEncoding (Builder.byteString bs)

instance FromPgField PgJson where
  fieldParser =
    FieldParser
      { fieldValueParser =
          \ColumnInfo {typeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if typeOid == jsonbOid then BS.drop 1 else Prelude.id
             in \case
                  Just bs -> Right $ PgJson $ fixJsonb bs
                  Nothing -> Left "Cannot decode SQL null as the Haskell PgJson type. Use a `Maybe PgJson` if you want SQL nulls",
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . typeOid
      }

-- | A newtype wrapper to decode a JSON value with Aeson
-- into your type (from either json or jsonb), and to encode
-- to jsonb.
newtype Aeson a = Aeson {getAeson :: a}
  deriving (Eq, Show, Read, Typeable, Functor)

instance (FromJSON a) => FromPgField (Aeson a) where
  fieldParser =
    FieldParser
      { fieldValueParser =
          \ColumnInfo {typeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if typeOid == jsonbOid then BS.drop 1 else Prelude.id
             in \case
                  Just bs -> case Aeson.decodeStrict $ fixJsonb bs of
                    Just v -> Right $ Aeson v
                    Nothing -> Left "Failed to decode postgres JSON value into your `Aeson a` type. Are you sure it's proper JSON?"
                  Nothing -> Left "Cannot decode SQL null as a Haskell (Aeson a) type. Use a `Maybe (Aeson a)` if you want SQL nulls",
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . typeOid
      }

instance (ToJSON a) => ToPgField (Aeson a) where
  toTypeOid _ _ = Just jsonbOid
  toPgField _ (Aeson v) =
    NotNull $ BS.cons 1 (LBS.toStrict $ Aeson.encode v)
