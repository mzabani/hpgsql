module Database.PostgreSQL.Simple.HpgsqlUtils where

import Database.PostgreSQL.Simple.Types (Query (..))
import qualified HPgsql
import HPgsql.Encoding (ToPgRow (..))
import qualified HPgsql.Query as HPgsql

toHpgsqlQuery :: (ToPgRow q) => Query -> q -> HPgsql.Query
toHpgsqlQuery (Query qry) params = HPgsql.mkQueryWithQuestionMarks qry (toPgParams params)
