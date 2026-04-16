{-# LANGUAGE LambdaCase #-}

module Database.PostgreSQL.Simple.HpgsqlUtils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple.ToField (Action (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified Hpgsql
import Hpgsql.Builder (BinaryField)
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
