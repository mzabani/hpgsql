module DbUtils where

import Control.Exception.Safe (throw, try)
import Control.Monad
  ( forM_,
    void,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace
import HPgsql (ConnString (..), ConnectOpts (..), ErrorDetail (..), HPgConnection, IrrecoverableHpgsqlError (..), PostgresError (..), defaultConnectOpts, execute, execute_, withConnection, withConnectionOpts)
import System.Environment (getEnv)
import System.Mem (performGC)
import Test.Hspec

testConnInfo :: IO ConnString
testConnInfo = do
  portStr <- getEnv "PGPORT"
  hostname <- getEnv "PGHOST"
  database <- getEnv "PGDATABASE"
  user <- getEnv "PGUSER"
  pure ConnString {user, database, hostname, port = read portStr, password = "", options = ""}

aroundConn :: SpecWith HPgConnection -> Spec
aroundConn = around $ \act -> do
  connstr <- testConnInfo
  -- Use use a very low polling interval to hopefully exercise code paths
  -- where polling does not detect dead threads in the very first check,
  -- and a very low cancellation request resend interval also to test more
  -- aggressive interruption of our code, and to make tests run faster.
  withConnectionOpts defaultConnectOpts {killedThreadPollIntervalMs = 1, cancellationRequestResendIntervalMs = 10} connstr 10 act

pgErrorMustContain :: ByteString -> [(ErrorDetail, LBS.ByteString)] -> PostgresError -> Bool
pgErrorMustContain expectedStmt expected (PostgresError {pgErrorDetails, failedStatement}) = Map.fromList expected `Map.isSubmapOf` pgErrorDetails && expectedStmt `BS.isInfixOf` failedStatement

irrecoverableErrorMustContain :: [(ErrorDetail, LBS.ByteString)] -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorMustContain expected (IrrecoverableHpgsqlError {pgErrorDetails}) = Map.fromList expected `Map.isSubmapOf` pgErrorDetails

irrecoverableErrorWithMsg :: String -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorWithMsg expectedInfixMsg (IrrecoverableHpgsqlError {hpgsqlDetails}) = traceShowId expectedInfixMsg `List.isInfixOf` traceShowId hpgsqlDetails

withRollback :: HPgConnection -> IO a -> IO a
withRollback conn f = do
  execute_ conn "BEGIN"
  res <- try @_ @PostgresError f
  -- On a postgres error, we still need to ROLLBACK, then rethrow
  execute_ conn "ROLLBACK"
  case res of
    Left e -> throw e
    Right v -> pure v
