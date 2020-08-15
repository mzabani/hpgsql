module Main where

import Control.Concurrent (threadDelay)
import Control.DeepSeq
  ( NFData,
    force,
  )
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
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
import Data.Aeson
  ( FromJSON,
    eitherDecodeFileStrict,
  )
import Data.Int (Int64)
import Data.Maybe
  ( fromMaybe,
    isJust,
  )
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.Streaming as StreamingPostgresSimple
import GHC.Generics (Generic)
import qualified HPgsql
import HPgsql.Connection (libpqConnString)
import Statistics.Regression (olsRegress)
import Streaming (Of (..))
import Streaming.Prelude (Stream)
import qualified Streaming.Prelude as S
import System.Environment (getEnv, lookupEnv)
import qualified System.IO as IO
import System.Mem (performBlockingMajorGC)
import Test.Hspec
  ( HasCallStack,
    describe,
    expectationFailure,
    hspec,
    it,
    runIO,
    shouldBe,
  )
import Test.Hspec.Expectations (shouldReturn)

testConnInfo :: IO HPgsql.ConnString
testConnInfo = getEnv "PGPORT" >>= \portStr -> pure HPgsql.ConnString {user = "postgres", database = "postgres", hostname = "127.0.0.1", port = read portStr, password = "", options = ""}

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
            runRepeatedly = \_ n -> forM_ [1 .. n] $ const $ do
              !_ <- superForce f
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
      ++ show ((/ (1024 * 1024)) . fromIntegral @Int64 @Double <$> fromInt measAllocated)
      ++ " MB."
  pure msr

shouldBeF :: (HasCallStack) => String -> Double -> Double -> Double -> IO ()
shouldBeF errorMsg tolerance actual expected =
  unless (abs (actual - expected) <= tolerance) $
    expectationFailure $
      errorMsg
        ++ ": Expected a number within "
        ++ show tolerance
        ++ " of "
        ++ show expected
        ++ " but got "
        ++ show actual

-- | With data that has all y values being the same, r2 is often calculated to be one of [-Infinity, Infinity, NaN].
-- I haven't seen it happen in other conditions yet, and https://github.com/haskell/statistics/issues/111 seems
-- to be slightly related, so I'm assuming that's the only case when it happens.
r2ShouldBe1 :: (HasCallStack) => String -> Double -> IO ()
r2ShouldBe1 errorMsg r2 =
  unless (isNaN r2 || isInfinite r2) $ shouldBeF errorMsg 0.001 r2 1

fromSlopeAndIntercept :: V.Vector Double -> (Double, Double)
fromSlopeAndIntercept v = case V.toList v of
  [slope, yIntercept] -> (slope, yIntercept)
  _ -> error "Not a two-element vector to extract slope and intercept"

-- | Checks that the measured quantities approximately form a line that passes
-- through (0,0).
mustFormALineWithOrigin ::
  [(Double, Measured)] -> (Measured -> Double) -> IO ()
mustFormALineWithOrigin ms f = do
  let xs = V.fromList $ map fst ms
      ys = V.fromList $ map (f . snd) ms
      (fromSlopeAndIntercept -> (_slope, yIntercept), r2) = olsRegress [xs] ys
  -- print xs
  -- print ys
  -- print (_slope, yIntercept, r2)
  shouldBeF "Line with origin (yIntercept)" 0.1 yIntercept 0
  r2ShouldBe1 "Line with origin (r^2)" r2

-- | Checks that the measured quantities approximately form a horizontal line.
mustFormAHorizontalLine ::
  [(Double, Measured)] -> (Measured -> Double) -> IO ()
mustFormAHorizontalLine ms f = do
  let xs = V.fromList $ map fst ms
      ys = V.fromList $ map (f . snd) ms
      (fromSlopeAndIntercept -> (slope, _yIntercept), _r2) = olsRegress [xs] ys
  -- print xs
  -- print ys
  -- print (slope, _yIntercept, r2)
  -- The error tolerance for the slope check is very stringent because so far
  -- we only use it for the peak memory usage test, and those are Ints in Megabytes
  -- so they really shouldn't beyond +- 1MB or something's wrong.
  -- If more tests use this function, we should probably make the tolerance
  -- an argument of it.
  shouldBeF "Horizontal line (slope)" 0.000_01 slope 0

fromGcInt :: Int64 -> Double
fromGcInt =
  fromIntegral
    . fromMaybe
      ( error
          "GC stats not enabled. Are you running benchmarks with +RTS -T ?"
      )
    . fromInt

-- | Given a list with total of some measure (e.g. time) per number of runs, returns the average measure (e.g. time) per individual run. One example is where each run is parsing of a SQL statement, but each test parses thousands of them.
avgSamples :: (Measured -> Double) -> [(Double, Measured)] -> Double
avgSamples _ [] = error "Average of empty list"
avgSamples f xs = sum (map (f . snd) xs) / sum (map fst xs) -- No need to fret with the implementation: our lists are small

data CurrentPerf = CurrentPerf
  { select1TimeAndMemory :: (Double, Double),
    copyTimeAndMemory :: (Double, Double)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.NoBuffering

  -- initializeTime must run first: https://hackage.haskell.org/package/criterion-measurement-0.2.0.0/docs/Criterion-Measurement.html#v:initializeTime
  initializeTime

  -- This is very ad-hoc, but variance in GitHub actions is much greater than on my own machine,
  -- and I don't want to increase variance tolerance on my machine as it's good for it to be tight.
  -- So we check if we're in CI and increase our tolerance for divergent perf numbers.
  isCI <- isJust <$> lookupEnv "CI"
  let expectedPerfPath :: FilePath =
        if isCI
          then "hpgsql-benchmarks/expected-perf/ci.json"
          else "hpgsql-benchmarks/expected-perf/local.json"
  putStrLn $ "Reading expected performance numbers from " ++ expectedPerfPath
  CurrentPerf {..} <-
    either (error "Could not decode expected performance file") id
      <$> eitherDecodeFileStrict expectedPerfPath

  let (diffWallTimeTolerance, diffMaxMemTolerance) =
        if isCI then (1e-4, 1) else (1e-5, 1)

  hspec $ do
    describe
      "Elapsed wall time must be linear with input size and max memory usage constant when streaming parse"
      $ do
        -- 1. Linear CPU time on input size means we're not accumulating anything
        --    unreasonably when parsing in streaming fashion.
        -- 2. Max memory usage constant is a must in a streaming parser as we should
        --    be able to parse arbitrarily large result sets.
        --    Since peak MB allocated is an integer, however, it could be that it rounds to different numbers
        --    on different runs despite the actual total max allocates bytes being very similar. This could
        --    mess up our horizontal line check.
        describe "Parsing Int64s into a List" $ do
          runIO $ do
            hpgsqlConnInfo <- testConnInfo
            conn <-
              HPgsql.connect hpgsqlConnInfo 10
            pgSimpleConn <- PGSimple.connectPostgreSQL (libpqConnString hpgsqlConnInfo)
            forM_ [10_000 :: Int, 100_000, 500_000, 1_000_000] $
              \n -> do
                bench "hpgsql" $
                  HPgsql.queryWith (HPgsql.rowParser @(HPgsql.Only Int)) conn (HPgsql.mkQuery "SELECT * FROM generate_series(1,$1)" (HPgsql.Only n))
            forM_ [10_000 :: Int, 100_000, 500_000, 1_000_000] $
              \n ->
                bench "postgresql-simple" $
                  PGSimple.query @_ @(PGSimple.Only Int) pgSimpleConn "SELECT * FROM generate_series(1,?)" (PGSimple.Only n)
        describe "Parsing Int64s in streaming fashion" $ do
          runIO $ do
            hpgsqlConnInfo <- testConnInfo
            conn <-
              HPgsql.connect hpgsqlConnInfo 10
            pgSimpleConn <- PGSimple.connectPostgreSQL (libpqConnString hpgsqlConnInfo)
            forM_ [10_000 :: Int, 100_000, 500_000, 1_000_000] $
              \n -> do
                bench "hpgsql" $ do
                  res <- HPgsql.queryWithStreaming (HPgsql.rowParser @(HPgsql.Only Int)) conn (HPgsql.mkQuery "SELECT * FROM generate_series(1,$1)" (HPgsql.Only n))
                  S.effects res
            forM_ [10_000 :: Int, 100_000, 500_000, 1_000_000] $
              \n ->
                bench "streaming-postgresql-simple" $ do
                  runResourceT @IO $ do
                    let res :: Stream (Of (PGSimple.Only Int)) (ResourceT IO) () = StreamingPostgresSimple.query pgSimpleConn "SELECT * FROM generate_series(1,?)" (PGSimple.Only n)
                    S.effects res

-- it "Elapsed wall time must linear wrt input size" $
--   mustFormALineWithOrigin rs measTime

--   it
--     "Peak memory usage must be constant regardless of input size"
--     $ mustFormAHorizontalLine
--       rs
--       (fromGcInt . measPeakMbAllocated)
--   it "Must not be too different from expected performance" $ do
--     shouldBeF
--       "Average wall time regression"
--       diffWallTimeTolerance
--       (avgSamples measTime rs)
--       (fst select1TimeAndMemory)
--     shouldBeF
--       "Max memory usage regression"
--       diffMaxMemTolerance
--       ( maximum $
--           map (fromGcInt . measPeakMbAllocated . snd) rs
--       )
--       (snd select1TimeAndMemory)

-- describe "Parsing COPY statement" $ do
--   rs :: [(Double, Measured)] <-
--     runIO $
--       forM [10_000, 100_000, 500_000, 1_000_000] $ -- Only large-ish sizes or less memory will be used and we want constant memory usage
--         \n ->
--           fmap (fromIntegral n,) $
--             bench ("COPY " ++ show n) $
--               do
--                 -- Uncomment the line below to make memory usage linear on input size
--                 -- and see memory usage rise and the test fail
--                 -- _ <- evaluate $ force [1 .. n]
--                 parseMig (veryLargeCopy n)
--                   >>= Streaming.any_
--                     ( ==
--                         CommentPiece
--                           "Not in the stream"
--                     )

--   it "Elapsed wall time must linear wrt input size" $
--     mustFormALineWithOrigin rs measTime
--   it
--     "Peak memory usage must be constant regardless of input size"
--     $ mustFormAHorizontalLine
--       rs
--       (fromGcInt . measPeakMbAllocated)

--   it "Must not be too different from expected performance" $ do
--     shouldBeF
--       "Average wall time regression"
--       diffWallTimeTolerance
--       (avgSamples measTime rs)
--       (fst copyTimeAndMemory)
--     shouldBeF
--       "Max memory usage regression"
--       diffMaxMemTolerance
--       ( maximum $
--           map (fromGcInt . measPeakMbAllocated . snd) rs
--       )
--       (snd copyTimeAndMemory)

--   it
--     "Number of SQL pieces must be low to avoid too many round-trips"
--     $ do
--       -- The body of the contents inside COPY can be very large, so we don't want
--       -- small pieces coming out of the parser to avoid too many unnecessary round-trips.
--       -- Any changes to the parser will be detected by this test, but those should never
--       -- increase the number of parsed pieces too much.
--       len :: Int <-
--         parseMig (veryLargeCopy 500_000)
--           >>= Streaming.length_
--       len `shouldBe` 178
