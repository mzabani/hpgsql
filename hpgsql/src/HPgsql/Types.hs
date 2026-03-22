module HPgsql.Types
  ( Aeson (..),
    PgJson, -- Do not export ctor
    Values (..),
    valuesToQuery,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonInternal
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import HPgsql.Encoding (ColumnInfo (..), FieldParser (..), FromPgField (..), ToPgField (..), ToPgRow (..))
import HPgsql.Query (Query (..), SingleQuery (..))
import HPgsql.TypeInfo (Format (..), jsonOid, jsonbOid)

-- TODO: Write tests for these types!

newtype Values a = Values [a]

-- | Generates a query like @VALUES ($1,$2), ($3,$4)@ from a list of rows.
-- Can be embedded inside a @[sql|...|]@ quasiquote using @^{expr}@ syntax:
--
-- > [sql| INSERT INTO emp(id,name) ^{valuesToQuery (Values rows)} ON CONFLICT DO NOTHING |]
valuesToQuery :: (ToPgRow a) => Values a -> Query
valuesToQuery (Values []) = Query $ NE.singleton $ SingleQuery "" [] -- TODO this should be an error? Maybe force a NonEmpty list?
valuesToQuery (Values rows@(firstRow : _)) =
  let allParams = concatMap toPgParams rows
      numCols = length (toPgParams firstRow)
      rowPlaceholder :: Int -> Text
      rowPlaceholder startIdx =
        "(" <> Text.intercalate ", " ["$" <> Text.pack (show (startIdx + i)) | i <- [0 .. numCols - 1]] <> ")"
      valuesClause = Text.intercalate ", " [rowPlaceholder (rowIdx * numCols + 1) | rowIdx <- [0 .. length rows - 1]]
      fullSql = encodeUtf8 $ "VALUES " <> valuesClause
   in Query $ NE.singleton $ SingleQuery fullSql allParams

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
  fieldParser =
    FieldParser
      { fieldValueParser =
          \ColumnInfo {typeOid} ->
            let -- jsonb has a byte prepended to the contents and json does not
                !fixJsonb = if typeOid == jsonbOid then LBS.drop 1 else Prelude.id
             in \case
                  Just bs -> Right $ PgJson $ fixJsonb bs
                  Nothing -> Left "Cannot decode SQL null as the Haskell PgJson type. Use a `Maybe PgJson` if you want SQL nulls",
        fieldFmt = BinaryFmt,
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
                !fixJsonb = if typeOid == jsonbOid then LBS.drop 1 else Prelude.id
             in \case
                  Just bs -> case Aeson.decode $ fixJsonb bs of
                    Just v -> Right $ Aeson v
                    Nothing -> Left "Failed to decode postgres JSON value into your `Aeson a` type. Are you sure it's proper JSON?"
                  Nothing -> Left "Cannot decode SQL null as a Haskell (Aeson a) type. Use a `Maybe (Aeson a)` if you want SQL nulls",
        fieldFmt = BinaryFmt,
        allowedPgTypes = (`elem` [jsonOid, jsonbOid]) . typeOid
      }

instance (ToJSON a) => ToPgField (Aeson a) where
  toTypeOid _ = Just jsonbOid
  toPgField (Aeson v) = Just $ LBS.cons 1 (Aeson.encode v)
