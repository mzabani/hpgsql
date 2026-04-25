module Hpgsql.Query
  ( Query, -- Do not export constructor
    SingleQuery, -- Do not export constructor
    sql,
    sqlPrep,
    mkQueryInternal,
    breakQueryIntoStatements,
    mkQuery,
    escapeIdentifier,
    vALUES,
    preparedStatement,
    nonPreparedStatement,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Hpgsql.Builder (BinaryField (..))
import Hpgsql.Encoding (RowEncoder (..), ToPgRow (..))
import Hpgsql.InternalTypes (Query (..), SingleQuery (..), SingleQueryFragment (..), breakQueryIntoStatements)
import Hpgsql.QueryInternal (mkQuery, mkQueryInternal, sql, sqlPrep)
import Hpgsql.TypeInfo (EncodingContext, Oid)

-- | Escapes a database object identifier like a table name or a column name,
-- so it can be embedded  inside a @[sql|...|]@ quasiquote using @^{expr}@ syntax:
--
-- > [sql| SELECT ^{escapeIdentifier "çolumñ"} FROM ^{escapeIdentifier "sChEmA"}.some_table |]
escapeIdentifier :: ByteString -> Query
escapeIdentifier v = Query {queryString = [FragmentOfStaticSql "\"", FragmentOfStaticSql (doubleQuotes v), FragmentOfStaticSql "\""], queryParams = [], isPrepared = False}
  where
    doubleQuotes = BS.intercalate "\"\"" . BS.split 0x22 {- '"' -}

-- | Generates a query like @VALUES ($1,$2), ($3,$4)@ from a list of rows.
-- Can be embedded inside a @[sql|...|]@ quasiquote using @^{expr}@ syntax:
--
-- > [sql| INSERT INTO emp(id,name) ^{vALUES rows} ON CONFLICT DO NOTHING |]
vALUES :: forall a. (ToPgRow a) => [a] -> Query
vALUES [] =
  let rowOids = toRowEncoder.toTypeOids (Proxy @a)
      nullParams :: [EncodingContext -> (Maybe Oid, BinaryField)]
      nullParams = map (\f -> \encCtx -> (f encCtx, SqlNull)) rowOids
   in [sql|(SELECT * FROM (VALUES ^{commaSeparatedRowTuples [nullParams]}) _subq LIMIT 0)|]
vALUES rows =
  let allParams = map toRowEncoder.toPgParams rows
   in "VALUES " <> commaSeparatedRowTuples allParams

commaSeparatedRowTuples :: [[EncodingContext -> (Maybe Oid, BinaryField)]] -> Query
commaSeparatedRowTuples rowTuples =
  let (_, queryFragsPerRow) =
        List.mapAccumR
          ( \(!maxArgSoFar) singleRow ->
              let numParams = length singleRow
                  numberedArgs = map (QueryArgumentPlaceHolder . (+ maxArgSoFar)) [1 .. numParams]
               in (maxArgSoFar + numParams, FragmentOfStaticSql "(" : (List.intersperse (FragmentOfStaticSql ",") numberedArgs ++ [FragmentOfStaticSql ")"]))
          )
          0
          rowTuples
   in Query {queryString = mconcat $ List.intersperse [FragmentOfStaticSql ","] queryFragsPerRow, queryParams = mconcat rowTuples, isPrepared = False}

preparedStatement :: Query -> Query
preparedStatement Query {..} = Query {isPrepared = True, ..}

nonPreparedStatement :: Query -> Query
nonPreparedStatement Query {..} = Query {isPrepared = False, ..}
