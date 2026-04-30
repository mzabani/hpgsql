module Main where

import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.STM as STM
import Control.DeepSeq
  ( NFData (..),
    force,
    rnf,
  )
import Control.Exception (evaluate)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Criterion.Measurement
  ( initializeTime,
    measure,
    secs,
  )
import Criterion.Measurement.Types
  ( Benchmarkable (..),
    Measured (..),
    fromInt,
  )
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int32, Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (Day, UTCTime)
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.Copy as PGSimple
import qualified Database.PostgreSQL.Simple.Streaming as StreamingPostgresSimple
import GHC.Generics (Generic)
import GHC.Stats (getRTSStats, max_live_bytes, max_mem_in_use_bytes)
import qualified Hasql.Connection as HasqlConn
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnSetting
import qualified Hasql.Decoders as HasqlDec
import qualified Hasql.Encoders as HasqlEnc
import qualified Hasql.Session as HasqlSess
import qualified Hasql.Statement as HasqlStmt
import qualified Hpgsql
import Hpgsql.Connection (libpqConnString)
import qualified Hpgsql.Copy
import qualified Hpgsql.Query as Hpgsql
import qualified Hpgsql.Types as Hpgsql
import Numeric (showFFloat)
import Streaming (Of (..))
import Streaming.Prelude (Stream)
import qualified Streaming.Prelude as S
import System.Environment (getEnv)
import qualified System.IO as IO
import System.Mem (performBlockingMajorGC)
import Test.Hspec
  ( describe,
    it,
    runIO,
  )
import Test.Hspec.Core.Formatters.V2 (formatterToFormat, silent)
import Test.Hspec.Core.Runner (configFormat, defaultConfig, hspecWith)

-- Orphan NFData instances for large tuples (deepseq only provides up to 9-tuples)
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m) => NFData (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  rnf (a, b, c, d, e, f, g, h, i, j, k, l, m) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` rnf l `seq` rnf m

data BenchRow = BenchRow
  { brId :: !Int,
    brDate1 :: !Day,
    brDate2 :: !Day,
    brTimestamp1 :: !UTCTime,
    brTimestamp2 :: !UTCTime,
    brText1 :: !Text,
    brText2 :: !Text,
    brDouble1 :: !Double,
    brDouble2 :: !Double,
    brMaybeInt :: !(Maybe Int),
    brMaybeText :: !(Maybe Text),
    brMaybeDouble :: !(Maybe Double),
    brMaybeDay :: !(Maybe Day)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hpgsql.FromPgRow, PGSimple.FromRow)

data HasqlBenchRow = HasqlBenchRow
  { hbrId :: !Int32,
    hbrDate1 :: !Day,
    hbrDate2 :: !Day,
    hbrTimestamp1 :: !UTCTime,
    hbrTimestamp2 :: !UTCTime,
    hbrText1 :: !Text,
    hbrText2 :: !Text,
    hbrDouble1 :: !Double,
    hbrDouble2 :: !Double,
    hbrMaybeInt :: !(Maybe Int32),
    hbrMaybeText :: !(Maybe Text),
    hbrMaybeDouble :: !(Maybe Double),
    hbrMaybeDay :: !(Maybe Day)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData)

testConnInfo :: IO Hpgsql.ConnString
testConnInfo = do
  portStr <- getEnv "PGPORT"
  hostname <- getEnv "PGHOST"
  database <- getEnv "PGDATABASE"
  user <- getEnv "PGUSER"
  pure Hpgsql.ConnString {user, database, hostname, port = read portStr, password = "", options = ""}

superForce :: (NFData a) => IO a -> IO a
superForce vio = vio >>= evaluate . force

bench :: (NFData a) => String -> IO a -> IO Measured
bench name f = do
  msr@Measured {..} <-
    fst
      <$> measure
        Benchmarkable
          { allocEnv = \_ -> pure (),
            cleanEnv = \_ _ -> pure (),
            runRepeatedly = \_ n -> do
              forM_ [1 .. n] $ const $ do
                !_ <- superForce f
                pure ()
              performBlockingMajorGC,
            perRun = True -- If True, `runRepeatedly` is called with `n=1`. Not sure why this exists, though.
          }
        10
  putStrLn $
    "--- Benchmark "
      ++ name
      ++ ": Wall time="
      ++ secs measTime
      ++ ", total Haskell (does not include C heap) memory allocated="
      ++ maybe "N/A" (\v -> showFFloat (Just 1) v "") ((/ (1024 * 1024)) . fromIntegral @Int64 @Double <$> fromInt measAllocated)
      ++ " MB."
  pure msr

-- | Some of our benchmarks will run queries in concurrent connections.
-- This is how many to use.
numConcurrentConnections :: Int
numConcurrentConnections = 2

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.NoBuffering

  -- initializeTime must run first: https://hackage.haskell.org/package/criterion-measurement-0.2.0.0/docs/Criterion-Measurement.html#v:initializeTime
  initializeTime

  putStrLn "IMPORTANT: all measurements collected over 10 runs of each benchmark"
  when (numConcurrentConnections > 1) $ putStrLn $ "IMPORTANT: all benchmarks except COPY involve running the benchmarked query in " ++ show numConcurrentConnections ++ " connections in parallel"
  performBlockingMajorGC
  statsBefore <- getRTSStats
  hspecWith defaultConfig {configFormat = Just (formatterToFormat silent)} $ do
    describe "Parsing 13-column rows into a List" $ do
      let sql = "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,$1) g"
          pgSimpleSql = "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,?) g"
          hasqlListStmt =
            HasqlStmt.Statement
              sql
              (HasqlEnc.param (HasqlEnc.nonNullable HasqlEnc.int4))
              ( HasqlDec.rowList
                  ( (,,,,,,,,,,,,)
                      <$> HasqlDec.column (HasqlDec.nonNullable HasqlDec.int4)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.date)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.date)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.timestamptz)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.timestamptz)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.text)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.text)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.float8)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.float8)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.int4)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.text)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.float8)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.date)
                  )
              )
              True
          hasqlRecordListStmt =
            HasqlStmt.Statement
              sql
              (HasqlEnc.param (HasqlEnc.nonNullable HasqlEnc.int4))
              ( HasqlDec.rowList
                  ( HasqlBenchRow
                      <$> HasqlDec.column (HasqlDec.nonNullable HasqlDec.int4)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.date)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.date)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.timestamptz)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.timestamptz)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.text)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.text)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.float8)
                      <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.float8)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.int4)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.text)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.float8)
                      <*> HasqlDec.column (HasqlDec.nullable HasqlDec.date)
                  )
              )
              True
      forM_ [10_000 :: Int, 100_000] $ \n -> do
        it ("hpgsql Tuple List (" ++ show n ++ " rows)") $
          void $
            bench ("hpgsql Tuple List (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections hpgsqlConnect Hpgsql.closeGracefully $ \conn -> do
                Hpgsql.queryWith (Hpgsql.rowDecoder @(Int, Day, Day, UTCTime, UTCTime, Text, Text, Double, Double, Maybe Int, Maybe Text, Maybe Double, Maybe Day)) conn (Hpgsql.mkQuery sql (Hpgsql.Only n))
        it ("hasql Tuple List (" ++ show n ++ " rows)") $
          void $
            bench ("hasql Tuple List (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections hasqlConnect HasqlConn.release $ \hasqlConn -> do
                result <- HasqlSess.run (HasqlSess.statement (fromIntegral n :: Int32) hasqlListStmt) hasqlConn
                either (\e -> error $ "hasql query failed: " ++ show e) pure result
        it ("postgresql-simple Tuple List (" ++ show n ++ " rows)") $
          void $
            bench ("postgresql-simple Tuple List (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections pgSimpleConnect PGSimple.close $ \pgSimpleConn -> do
                PGSimple.query @_ @(Int, Day, Day, UTCTime, UTCTime, Text, Text, Double, Double, Maybe Int, Maybe Text, Maybe Double, Maybe Day) pgSimpleConn pgSimpleSql (PGSimple.Only n)
        it ("hpgsql Record List (" ++ show n ++ " rows)") $
          void $
            bench ("hpgsql Record List (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections hpgsqlConnect Hpgsql.closeGracefully $ \conn -> do
                Hpgsql.queryWith (Hpgsql.rowDecoder @BenchRow) conn (Hpgsql.mkQuery sql (Hpgsql.Only n))
        it ("hasql Record List (" ++ show n ++ " rows)") $
          void $
            bench ("hasql Record List (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections hasqlConnect HasqlConn.release $ \hasqlConn -> do
                result <- HasqlSess.run (HasqlSess.statement (fromIntegral n :: Int32) hasqlRecordListStmt) hasqlConn
                either (\e -> error $ "hasql query failed: " ++ show e) pure result
        it ("postgresql-simple Record List (" ++ show n ++ " rows)") $
          void $
            bench ("postgresql-simple Record List (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections pgSimpleConnect PGSimple.close $ \pgSimpleConn -> do
                PGSimple.query @_ @BenchRow pgSimpleConn pgSimpleSql (PGSimple.Only n)
    describe "Parsing 13-column rows in streaming fashion" $ do
      let sql = "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,$1) g"
      forM_ [10_000 :: Int, 100_000] $ \n -> do
        it ("hpgsql Tuple Stream (" ++ show n ++ " rows)") $
          void $
            bench ("hpgsql Tuple Stream (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections hpgsqlConnect Hpgsql.closeGracefully $ \conn -> do
                res <- Hpgsql.querySWith (Hpgsql.rowDecoder @(Int, Day, Day, UTCTime, UTCTime, Text, Text, Double, Double, Maybe Int, Maybe Text, Maybe Double, Maybe Day)) conn (Hpgsql.mkQuery sql (Hpgsql.Only n))
                S.effects res
        it ("streaming-postgresql-simple Tuple Stream (" ++ show n ++ " rows)") $
          void $
            bench ("streaming-postgresql-simple Tuple Stream (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections pgSimpleConnect PGSimple.close $ \pgSimpleConn -> do
                runResourceT @IO $ do
                  let res :: Stream (Of (Int, Day, Day, UTCTime, UTCTime, Text, Text, Double, Double, Maybe Int, Maybe Text, Maybe Double, Maybe Day)) (ResourceT IO) () = StreamingPostgresSimple.query pgSimpleConn "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,?) g" (PGSimple.Only n)
                  S.effects res
        it ("postgresql-simple Tuple fold (" ++ show n ++ " rows)") $
          void $
            bench ("postgresql-simple Tuple fold (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections pgSimpleConnect PGSimple.close $ \pgSimpleConn -> do
                PGSimple.fold pgSimpleConn "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,?) g" (PGSimple.Only n) () (\() (!_ :: (Int, Day, Day, UTCTime, UTCTime, Text, Text, Double, Double, Maybe Int, Maybe Text, Maybe Double, Maybe Day)) -> pure ())
        it ("hpgsql Record Stream (" ++ show n ++ " rows)") $
          void $
            bench ("hpgsql Record Stream (" ++ show n ++ " rows)") $ do
              withMultipleConnections numConcurrentConnections hpgsqlConnect Hpgsql.closeGracefully $ \conn -> do
                res <- Hpgsql.querySWith (Hpgsql.rowDecoder @BenchRow) conn (Hpgsql.mkQuery sql (Hpgsql.Only n))
                S.effects res
        it ("streaming-postgresql-simple Record Stream (" ++ show n ++ " rows)") $
          void $
            bench ("streaming-postgresql-simple Record Stream (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections pgSimpleConnect PGSimple.close $ \pgSimpleConn -> do
                runResourceT @IO $ do
                  let res :: Stream (Of BenchRow) (ResourceT IO) () = StreamingPostgresSimple.query pgSimpleConn "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,?) g" (PGSimple.Only n)
                  S.effects res
        it ("postgresql-simple Record fold (" ++ show n ++ " rows)") $
          void $
            bench ("postgresql-simple Record fold (" ++ show n ++ " rows)") $
              withMultipleConnections numConcurrentConnections pgSimpleConnect PGSimple.close $ \pgSimpleConn -> do
                PGSimple.fold pgSimpleConn "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), 'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, NULL::int4, NULL::text, NULL::float8, NULL::date FROM generate_series(1,?) g" (PGSimple.Only n) () (\() (!_ :: BenchRow) -> pure ())
    describe "COPY FROM STDIN" $ do
      (conn, pgSimpleConn) <- runIO $ do
        hpgsqlConnInfo <- testConnInfo
        conn <- Hpgsql.connect hpgsqlConnInfo 10
        pgSimpleConn <- PGSimple.connectPostgreSQL (libpqConnString hpgsqlConnInfo)
        pure (conn, pgSimpleConn)
      let createCopyBenchTable :: (IsString s) => s
          createCopyBenchTable = "CREATE UNLOGGED TABLE copy_bench (id INT4 NOT NULL, name TEXT NOT NULL, item TEXT NOT NULL, value FLOAT8 NOT NULL)"
          mkRow g = (g :: Int32, "some-text" :: Text, "some-other-text" :: Text, fromIntegral g * 1.5 :: Double)
      forM_ [100_000 :: Int] $ \n -> do
        it ("hpgsql copyFromS binary COPY (" ++ show n ++ " rows)") $
          void $
            bench ("hpgsql copyFromS binary COPY (" ++ show n ++ " rows)") $ do
              Hpgsql.execute_ conn "BEGIN"
              Hpgsql.execute_ conn createCopyBenchTable
              void $
                Hpgsql.Copy.copyFromS conn "COPY copy_bench FROM STDIN WITH (FORMAT BINARY)" $
                  S.map
                    mkRow
                    (S.each [1 .. fromIntegral n])
              Hpgsql.execute_ conn "ROLLBACK"
        it ("postgresql-simple text COPY (" ++ show n ++ " rows)") $
          void $
            bench ("postgresql-simple text COPY (" ++ show n ++ " rows)") $ do
              void $ PGSimple.execute_ pgSimpleConn "BEGIN"
              void $ PGSimple.execute_ pgSimpleConn createCopyBenchTable
              PGSimple.copy_ pgSimpleConn "COPY copy_bench FROM STDIN WITH (FORMAT CSV)"
              forM_ (map mkRow [1 .. fromIntegral n]) $ \(g, t1, t2, g2) ->
                PGSimple.putCopyData pgSimpleConn $
                  BS8.pack (show g) <> "," <> encodeUtf8 t1 <> "," <> encodeUtf8 t2 <> "," <> BS8.pack (show g2) <> "\n"
              void $ PGSimple.putCopyEnd pgSimpleConn
              void $ PGSimple.execute_ pgSimpleConn "ROLLBACK"
  performBlockingMajorGC
  statsAfter <- getRTSStats
  let peakBefore = max_mem_in_use_bytes statsBefore
      peakAfter = max_mem_in_use_bytes statsAfter
      liveBefore = max_live_bytes statsBefore
      liveAfter = max_live_bytes statsAfter
      toMB n = showFFloat (Just 1) (fromIntegral n / (1024 * 1024) :: Double) ""
  putStrLn $ "--- Peak memory (max_mem_in_use_bytes): " ++ toMB (peakAfter - peakBefore) ++ " M"
  putStrLn $ "--- Peak live data (max_live_bytes): " ++ toMB (liveAfter - liveBefore) ++ " M"

withMultipleConnections :: Int -> IO conn -> (conn -> IO ()) -> (conn -> IO a) -> IO [a]
withMultipleConnections n acquireConn closeConn f = do
  waitAll <- STM.newTVarIO n
  mapConcurrently (benchF waitAll) [1 .. n]
  where
    benchF waitAll = const $ do
      conn <- acquireConn
      STM.atomically $ do
        left <- STM.readTVar waitAll
        STM.writeTVar waitAll (left - 1)
      -- Wait until all connections are established to
      -- make benchmarked queries run more simultaneously
      STM.atomically $ do
        left <- STM.readTVar waitAll
        unless (left == 0) STM.retry
      res <- f conn
      closeConn conn
      pure res

hpgsqlConnect :: IO Hpgsql.HPgConnection
hpgsqlConnect = do
  hpgsqlConnInfo <- testConnInfo
  Hpgsql.connect hpgsqlConnInfo 10

pgSimpleConnect :: IO PGSimple.Connection
pgSimpleConnect = do
  hpgsqlConnInfo <- testConnInfo
  PGSimple.connectPostgreSQL (libpqConnString hpgsqlConnInfo)

hasqlConnect :: IO HasqlConn.Connection
hasqlConnect = do
  hpgsqlConnInfo <- testConnInfo
  either (\e -> error $ "hasql connection failed: " ++ show e) id
    <$> HasqlConn.acquire [HasqlSetting.connection (HasqlConnSetting.string (decodeUtf8 (libpqConnString hpgsqlConnInfo)))]
