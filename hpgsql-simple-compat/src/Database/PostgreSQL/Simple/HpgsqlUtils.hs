{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.PostgreSQL.Simple.HpgsqlUtils
  ( toHpgsqlQuery,
    toHpgsqlRowParams,
    toPgSimpleQuery,
    PgSimpleRow (..),
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import Database.PostgreSQL.Simple.ToField (Action (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified Hpgsql
import Hpgsql.Builder (BinaryField)
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
