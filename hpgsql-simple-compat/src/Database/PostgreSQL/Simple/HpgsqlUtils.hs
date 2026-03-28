{-# LANGUAGE LambdaCase #-}

module Database.PostgreSQL.Simple.HpgsqlUtils where

import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import Database.PostgreSQL.Simple.ToField (Action (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified HPgsql
import qualified HPgsql.Query as HPgsql
import HPgsql.TypeInfo (Oid, TypeInfo)

toHpgsqlQuery :: (ToRow q) => Query -> q -> HPgsql.Query
toHpgsqlQuery (Query qry) row = HPgsql.mkQueryInternal qry (toHpgsqlRowParams row)

toHpgsqlRowParams :: (ToRow q) => q -> [Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)]
toHpgsqlRowParams = concatMap actionToPgParams . toRow
  where
    actionToPgParams :: Action -> [Map Oid TypeInfo -> (Maybe Oid, Maybe LBS.ByteString)]
    actionToPgParams = \case
      QueryArgument qa -> [qa]
      EscapeIdentifier _ -> [] -- These should be encoded into the query string directly
      Many actions -> concatMap actionToPgParams actions
