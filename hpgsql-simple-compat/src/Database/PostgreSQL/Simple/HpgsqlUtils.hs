{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.PostgreSQL.Simple.HpgsqlUtils
  ( toHpgsqlQuery,
    toHpgsqlRowParams,
    toPgSimpleQuery,
    Conversion (..),
    PgSimpleRow (..),
    toHpgsqlFieldDecoder,
    fromHpgsqlFieldDecoder,
    Field,
    FieldParser,
  )
where

import Control.Exception (toException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import Database.PostgreSQL.Simple.Ok (Ok (..))
import Database.PostgreSQL.Simple.ToField (Action (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified Hpgsql
import Hpgsql.Builder (BinaryField)
import Hpgsql.Encoding (ColumnInfo (..), FieldDecoder (..))
import Hpgsql.InternalTypes (SingleQueryFragment (..))
import qualified Hpgsql.InternalTypes as HpgsqlTypes
import qualified Hpgsql.Query as Hpgsql
import Hpgsql.TypeInfo (EncodingContext, Oid)

toHpgsqlQuery :: (ToRow q) => Query -> q -> Hpgsql.Query
toHpgsqlQuery (Query qry) row = Hpgsql.mkQueryInternal qry (toHpgsqlRowParams row)

toHpgsqlRowParams :: (ToRow q) => q -> [[Either ByteString (EncodingContext -> (Maybe Oid, BinaryField))]]
toHpgsqlRowParams = concatMap actionToPgParams . toRow
  where
    actionToPgParams :: Action -> [[Either ByteString (EncodingContext -> (Maybe Oid, BinaryField))]]
    actionToPgParams = \case
      Plain sql -> [[Left $ LBS.toStrict sql]] -- Static SQL fragments go to the query string directly
      QueryArgument qa -> [[Right qa]]
      EscapeIdentifier ident -> [[Left $ escapeIdentifier ident]] -- These should be encoded into the query string directly
      Many actions -> [mconcat $ concatMap actionToPgParams actions]

escapeIdentifier :: ByteString -> ByteString
escapeIdentifier v = "\"" <> BS.intercalate "\"\"" (BS.split 0x22 {- '"' -} v) <> "\""

newtype PgSimpleRow = PgSimpleRow [Action]
  deriving newtype (ToRow)

-- |  This is different from postgresql-simple's `Conversion` type, but it's the
-- best we can do, because hpgsql-simple-compat's decoding cannot run IO.
newtype Conversion a = Conversion {runConversion :: EncodingContext -> Ok a}

-- liftConversion :: IO a -> Conversion a
-- liftConversion m = Conversion (\_ -> Ok <$> m)

instance Functor Conversion where
  fmap f m = Conversion $ \encCtx -> fmap f (runConversion m encCtx)

instance Applicative Conversion where
  pure a = Conversion $ \_conn -> pure a
  mf <*> ma = Conversion $ \conn -> do
    case runConversion mf conn of
      Ok f -> fmap f (runConversion ma conn)
      Errors errs -> (Errors errs)

-- instance Alternative Conversion where
--   empty = Conversion $ \_conn -> pure empty
--   ma <|> mb = Conversion $ \conn -> do
--     oka <- runConversion ma conn
--     case oka of
--       Ok _ -> return oka
--       Errors _ -> (oka <|>) <$> runConversion mb conn

instance Monad Conversion where
  m >>= f = Conversion $ \conn -> do
    case runConversion m conn of
      Ok a -> runConversion (f a) conn
      Errors err -> Errors err

-- instance MonadPlus Conversion where
--   mzero = empty
--   mplus = (<|>)

-- conversionMap :: (Ok a -> Ok b) -> Conversion a -> Conversion b
-- conversionMap f m = Conversion $ \conn -> f <$> runConversion m conn

-- conversionError :: (Exception err) => err -> Conversion a
-- conversionError err = Conversion $ \_ -> return (Errors [toException err])

type Field = ColumnInfo

type FieldParser a = Field -> Maybe ByteString -> Conversion a

toHpgsqlFieldDecoder :: FieldParser a -> FieldDecoder a
toHpgsqlFieldDecoder fp =
  FieldDecoder
    { fieldValueDecoder = \colInfo mbs ->
        let valConv = fp colInfo mbs
         in case runConversion valConv colInfo.encodingContext of
              Ok v -> Right v
              Errors errs -> Left (show errs),
      allowedPgTypes = const True -- No way to check if types are valid ahead of time
    }

fromHpgsqlFieldDecoder :: FieldDecoder a -> FieldParser a
fromHpgsqlFieldDecoder dec = \f mbs -> Conversion $ \_encCtx -> case dec.fieldValueDecoder f mbs of
  Right v -> Ok v
  Left err -> Errors [toException $ userError $ show err]

-- | Given a Hpgsql query, returns the text format with question marks
-- for query arguments and a row object. With both, you can call
-- hpgsql-simple-compat's querying functions as you normally would.
toPgSimpleQuery :: Hpgsql.Query -> (Query, PgSimpleRow)
toPgSimpleQuery hpgsqlQuery =
  let paramsMap = IntMap.fromList $ zip [1 ..] hpgsqlQuery.queryParams
      go frag (ts, as) = case frag of
        FragmentOfStaticSql bs -> (bs : ts, as)
        FragmentWithSemiColon -> (";" : ts, as)
        FragmentOfCommentsOrWhitespace bs -> (bs : ts, as)
        QueryArgumentPlaceHolder n -> case IntMap.lookup n paramsMap of
          Just p -> ("?" : ts, QueryArgument p : as)
          Nothing -> error $ "toPgSimpleQuery: query argument placeholder $" ++ show n ++ " has no corresponding parameter"
      (textParts, actions) = foldr go ([], []) hpgsqlQuery.queryString
      queryText = mconcat textParts
   in (Query queryText, PgSimpleRow actions)
