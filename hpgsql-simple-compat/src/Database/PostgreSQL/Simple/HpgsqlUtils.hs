{-# LANGUAGE LambdaCase #-}

module Database.PostgreSQL.Simple.HpgsqlUtils where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple.ToField (Action (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified HPgsql
import qualified HPgsql.Query as HPgsql
import HPgsql.TypeInfo (EncodingContext, Oid)

toHpgsqlQuery :: (ToRow q) => Query -> q -> HPgsql.Query
toHpgsqlQuery (Query qry) row = HPgsql.mkQueryInternal qry (toHpgsqlRowParams row)

toHpgsqlRowParams :: (ToRow q) => q -> [Either ByteString (EncodingContext -> (Maybe Oid, Maybe LBS.ByteString))]
toHpgsqlRowParams = concatMap actionToPgParams . toRow
  where
    actionToPgParams :: Action -> [Either ByteString (EncodingContext -> (Maybe Oid, Maybe LBS.ByteString))]
    actionToPgParams = \case
      Plain sql -> [Left $ LBS.toStrict sql] -- Static SQL fragments go to the query string directly
      QueryArgument qa -> [Right qa]
      EscapeIdentifier ident -> [Left $ escapeIdentifier ident] -- These should be encoded into the query string directly
      Many actions -> concatMap actionToPgParams actions

escapeIdentifier :: ByteString -> ByteString
-- TODO THIS IS NOT SAFE! Implement this properly!
escapeIdentifier v = "\"" <> v <> "\""
