{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

{-

Testing strategies:

fromString . toString == id           ** Todo?

toString . fromString == almost id    ** Todo?

postgresql -> haskell -> postgresql   *  Done

haskell -> postgresql -> haskell      ** Todo?

But still,  what we really want to establish is that the two values
correspond;  for example,  a conversion that consistently added hour
when printed to a string and subtracted an hour when parsed from string
would still pass these tests.

Right now,  we are checking that 1400+ timestamps in the range of 1860 to
2060 round trip from postgresql to haskell and back in 5 different timezones.
In addition to UTC,  the four timezones were selected so that 2 have a positive
offset,  and 2 have a negative offset,   and that 2 have an offset of a
whole number of hours,  while the other two do not.

It may be worth adding a few more timezones to ensure better test coverage.

We are checking a handful of selected timestamps to ensure we hit
various corner-cases in the code,  in addition to 1400 timestamps randomly
generated with granularity of seconds down to microseconds in powers of ten.

-}

module Time (testTime) where

import Common
import Control.Monad (forM_, replicateM_)
import Data.Time.Compat
import Database.PostgreSQL.Simple.SqlQQ

numTests :: Int
numTests = 200

testTime :: TestEnv -> Assertion
testTime env@TestEnv {..} = do
  initializeTable env
  execute_ conn "SET timezone TO 'UTC'"
  checkRoundTrips env (read @UTCTime "1860-01-01 00:00:00 UTC")
  execute_ conn "SET timezone TO 'America/Chicago'" -- -5:00
  checkRoundTrips env (read @UTCTime "1883-11-18 18:00:00 UTC")
  execute_ conn "SET timezone TO 'Asia/Tokyo'" -- +9:00
  checkRoundTrips env (read @UTCTime "1887-12-31 15:00:00 UTC")
  execute_ conn "SET timezone TO 'Asia/Kathmandu'" -- +5:45
  checkRoundTrips env (read @UTCTime "1919-12-31 18:18:44 UTC")
  execute_ conn "SET timezone TO 'America/St_Johns'" -- -3:30
  checkRoundTrips env (read @UTCTime "1935-03-30 03:30:52 UTC")

initializeTable :: TestEnv -> IO ()
initializeTable TestEnv {..} = withTransaction conn $ do
  execute_
    conn
    [sql| CREATE TEMPORARY TABLE testtime
             ( x serial, y timestamptz, PRIMARY KEY(x) ) |]

  let test :: ZonedTime -> IO () = \x -> do
        execute
          conn
          [sql|
                   INSERT INTO testtime (y) VALUES (?)
                |]
          (Only x)
        return ()
      addSeconds :: NominalDiffTime -> ZonedTime -> ZonedTime
      addSeconds s zt =
        zt {zonedTimeToLocalTime = addLocalTime s (zonedTimeToLocalTime zt)}
  -- America/Chicago
  test (addSeconds 36 $ read @ZonedTime "1883-11-18 11:59:59 -0550")
  test (addSeconds 36 $ read @ZonedTime "1883-11-18 12:09:23 -0550")
  test (read @ZonedTime "1883-11-18 12:00:00 -0600")
  -- Asia/Tokyo
  test (addSeconds (-59) $ read @ZonedTime "1887-12-31 23:59:59 +0918")
  test (addSeconds (-59) $ read @ZonedTime "1888-01-01 00:18:58 +0918")
  test (read @ZonedTime "1888-01-01 00:00:00 +0900")
  -- Asia/Kathmandu
  test (addSeconds (-16) $ read @ZonedTime "1919-12-31 23:59:59 +0541")
  test (read @ZonedTime "1919-12-31 23:48:44 +0530")
  test (read @ZonedTime "1985-12-31 23:59:59 +0530")
  test (read @ZonedTime "1986-01-01 00:15:00 +0545")
  -- America/St_Johns
  test (addSeconds 52 $ read @ZonedTime "1935-03-29 23:59:59 -0330")
  test (read @ZonedTime "1935-03-30 00:00:52 -0330")

  -- While the above special cases are probably a decent start,  there
  -- are probably more that are well worth adding to ensure better
  -- coverage.

  let pop :: CalendarDiffTime -> Double -> IO () = \x y ->
        replicateM_ numTests $
          execute
            conn
            [sql| INSERT INTO testtime (y) VALUES
                         ('1860-01-01 00:00:00+00'::timestamptz
                          + ?::interval * ROUND(RANDOM() * ?)) |]
            (x, y)
  pop (CalendarDiffTime 0 0.000001) 6.3113904e15
  pop (CalendarDiffTime 0 0.00001) 6.3113904e14
  pop (CalendarDiffTime 0 0.0001) 6.3113904e13
  pop (CalendarDiffTime 0 0.001) 6.3113904e12
  pop (CalendarDiffTime 0 0.01) 6.3113904e11
  pop (CalendarDiffTime 0 0.1) 6.3113904e10
  pop (CalendarDiffTime 0 1) 6.3113904e9

checkRoundTrips :: TestEnv -> UTCTime -> IO ()
checkRoundTrips TestEnv {..} limit = do
  yxs :: [(UTCTime, Int)] <- query_ conn [sql| SELECT y, x FROM testtime |]
  forM_ yxs $ \yx -> do
    res <- query conn [sql| SELECT y=? FROM testtime WHERE x=? |] yx
    assertBool "UTCTime did not round-trip from SQL to Haskell and back" $
      res == [Only True]

  yxs :: [(ZonedTime, Int)] <-
    query
      conn
      [sql| 
              SELECT y, x FROM testtime WHERE y > ? 
           |]
      (Only limit)
  forM_ yxs $ \yx -> do
    res <- query conn [sql| SELECT y=? FROM testtime WHERE x=? |] yx
    assertBool "ZonedTime did not round-trip from SQL to Haskell and back" $
      res == [Only True]
