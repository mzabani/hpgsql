module DbUtils where

import Control.Exception.Safe (fromException, throw, try)
import Control.Monad
  ( forM_,
    void,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Hpgsql (ConnString (..), ConnectOpts (..), ErrorDetail (..), HPgConnection, IrrecoverableHpgsqlError (..), PostgresError (..), execute, execute_)
import Hpgsql.Connection (defaultConnectOpts, withConnection, withConnectionOpts)
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
  -- where polling does not detect dead threads in the very first check.
  -- A very low cancellation request resend interval is dangerous, however,
  -- because it can prevent orphaned query draining from ever completing.
  -- We recommend >=100ms in the docs, but tests seem to be fine with 10ms.
  withConnectionOpts defaultConnectOpts {killedThreadPollIntervalMs = 1, cancellationRequestResendIntervalMs = 10} connstr 10 act

pgErrorMustContain :: ByteString -> [(ErrorDetail, LBS.ByteString)] -> PostgresError -> Bool
pgErrorMustContain expectedStmt expected (PostgresError {pgErrorDetails, failedStatement}) = Map.fromList expected `Map.isSubmapOf` pgErrorDetails && expectedStmt `BS.isInfixOf` failedStatement

irrecoverableErrorMustContain :: [(ErrorDetail, LBS.ByteString)] -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorMustContain expected (IrrecoverableHpgsqlError {innerException}) =
  let pgErrorDetails
        | Just (Just (PostgresError {pgErrorDetails})) <- fromException <$> innerException = pgErrorDetails
        | otherwise = mempty
   in Map.fromList expected `Map.isSubmapOf` pgErrorDetails

irrecoverableErrorWithMsg :: String -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorWithMsg expectedInfixMsg (IrrecoverableHpgsqlError {hpgsqlDetails}) = expectedInfixMsg `List.isInfixOf` hpgsqlDetails

irrecoverableErrorWithMsgAndStmt :: ByteString -> String -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorWithMsgAndStmt expectedInfixStmt expectedInfixMsg (IrrecoverableHpgsqlError {hpgsqlDetails, relatedStatement}) = expectedInfixMsg `List.isInfixOf` hpgsqlDetails && expectedInfixStmt `BS.isInfixOf` fromMaybe "" relatedStatement

withRollback :: HPgConnection -> IO a -> IO a
withRollback conn f = do
  execute_ conn "BEGIN"
  res <- try @_ @PostgresError f
  -- On a postgres error, we still need to ROLLBACK, then rethrow
  execute_ conn "ROLLBACK"
  case res of
    Left e -> throw e
    Right v -> pure v
