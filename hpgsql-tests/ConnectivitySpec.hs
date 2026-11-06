module ConnectivitySpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception.Safe (SomeException, try)
import qualified Data.List as List
import Data.Text (Text)
import DbUtils
  ( irrecoverableErrorMustContain,
    irrecoverableErrorWithMsg,
    testConnInfo,
  )
import Hpgsql
import Hpgsql.Cancellation (cancelActiveStatement)
import Hpgsql.Connection (connect, connectionIsClosed, getBackendPid, withConnection)
import Hpgsql.Internal.InternalTypes (ConnectOpts (..), ConnectionString (..))
import Hpgsql.Query (mkQuery, sql)
import Hpgsql.Types (Only (..))
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "Connectivity" $ parallel $ do
    it
      "Connecting to non-existing db"
      connectingToNonExistingDb
    it
      "Connecting with connect-time options"
      connectingWithConnectTimeOptions
    it
      "Trying to use a closed connection"
      tryingToUseClosedConnection
    it
      "Connecting and change to unsupported client_encoding"
      connectingAndChangeToUnsupportedClientEncoding
    it
      "Connecting with unencrypted password"
      connectingWithUnencryptedPassword
    it
      "Connecting with MD5 password"
      connectingWithMD5Password
    it
      "A connection whose backend was terminated mid-query throws instead of hanging"
      disconnectingMidQueryDoesNotHang
    it
      "Sending on a connection whose backend was already terminated throws instead of hanging"
      sendingOnABrokenConnectionDoesNotHang

connectingToNonExistingDb :: IO ()
connectingToNonExistingDb = do
  hpgsqlConnInfo <- testConnInfo
  connect hpgsqlConnInfo {database = "non-existing-db"} 10 `shouldThrow` irrecoverableErrorMustContain [(ErrorCode, "3D000"), (ErrorHumanReadableMsg, "database \"non-existing-db\" does not exist")]

connectingWithConnectTimeOptions :: IO ()
connectingWithConnectTimeOptions = do
  hpgsqlConnInfo <- testConnInfo
  withConnection hpgsqlConnInfo {options = "-c my.random_setting=4"} 10 $ \conn -> do
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
    execute_ conn "SET my.random_setting=7"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("7" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]

tryingToUseClosedConnection :: IO ()
tryingToUseClosedConnection = do
  hpgsqlConnInfo <- testConnInfo
  connUsed <- newEmptyMVar
  withConnection hpgsqlConnInfo 10 $ \conn -> do
    putMVar connUsed conn
    connectionIsClosed conn `shouldReturn` False

  conn <- readMVar connUsed
  connectionIsClosed conn `shouldReturn` True
  execute_ conn "ANY QUERY" `shouldThrow` irrecoverableErrorWithMsg "Attempting to use a database connection that has been closed"

connectingAndChangeToUnsupportedClientEncoding :: IO ()
connectingAndChangeToUnsupportedClientEncoding = do
  hpgsqlConnInfo <- testConnInfo
  withConnection hpgsqlConnInfo 10 $ \conn -> do
    query conn "SELECT current_setting('client_encoding')" `shouldReturn` [Only ("UTF8" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('client_encoding')" `shouldReturn` [Only ("UTF8" :: Text)]
    execute_ conn "SET client_encoding='SQL_ASCII'"
      `shouldThrow` irrecoverableErrorWithMsg "Postgres sent us a change of client_encoding to not UTF8, and Hpgsql only supports UTF8"

connectingWithUnencryptedPassword :: IO ()
connectingWithUnencryptedPassword = do
  hpgsqlConnInfo <- testConnInfo
  connect hpgsqlConnInfo {user = "user_pass", password = "WRONG-password"} 10 `shouldThrow` \(ex :: IrrecoverableHpgsqlError) -> "password authentication failed for user" `List.isInfixOf` show ex
  Only connectedUser <- withConnection hpgsqlConnInfo {user = "user_pass", password = "hpgsql-password"} 10 $ \conn -> do
    cancelActiveStatement conn False -- This requires connecting again, so it's a reasonable test
    query1 conn "SELECT current_user"
  connectedUser `shouldBe` ("user_pass" :: String)

connectingWithMD5Password :: IO ()
connectingWithMD5Password = do
  hpgsqlConnInfo <- testConnInfo
  connect hpgsqlConnInfo {user = "user_md5", password = "WRONG-password"} 10 `shouldThrow` \(ex :: IrrecoverableHpgsqlError) -> "password authentication failed for user" `List.isInfixOf` show ex
  Only connectedUser <- withConnection hpgsqlConnInfo {user = "user_md5", password = "hpgsql-password"} 10 $ \conn -> do
    cancelActiveStatement conn False -- This requires connecting again, so it's a reasonable test
    query1 conn "SELECT current_user"
  connectedUser `shouldBe` ("user_md5" :: String)

-- | This test exists because once hpgsql had a bug when it wasn't handling recv returning
-- 0 bytes properly. This bug might not happen ever again, but the test remains.
disconnectingMidQueryDoesNotHang :: IO ()
disconnectingMidQueryDoesNotHang = do
  hpgsqlConnInfo <- testConnInfo
  waitOutcomeVar <- newEmptyMVar
  -- The dying connection's own cleanup (on withConnection's exit) also tries to write to the
  -- now-broken socket and throws some other exception; that's expected and not what this test
  -- is checking, so it's discarded here rather than conflated with waitOutcomeVar's content.
  _ <- try @_ @SomeException $ withConnection hpgsqlConnInfo 10 $ \conn -> do
    let pid = getBackendPid conn
    withAsync (execute_ conn "SELECT pg_sleep(5)") $ \queryAsync -> do
      threadDelay 500_000 -- give the query time to be sent, so queryAsync blocks on recv
      withConnection hpgsqlConnInfo 10 $ \killerConn ->
        execute_ killerConn (mkQuery "SELECT pg_terminate_backend($1)" (Only pid))
      waitOutcome <- timeout 5_000_000 (try @_ @IrrecoverableHpgsqlError (wait queryAsync))
      putMVar waitOutcomeVar waitOutcome
  waitOutcome <- readMVar waitOutcomeVar
  case waitOutcome of
    Nothing -> expectationFailure "Query on a connection whose backend was terminated hung instead of throwing"
    Just (Right ()) -> expectationFailure "Query succeeded even though its backend was terminated mid-query"
    Just (Left ex) -> ex `shouldSatisfy` irrecoverableErrorWithMsg "Remote peer has closed the connection"

-- | This test exists because hpgsql once ignored errors from the non-blocking send() call.
-- This bug might not happen ever again, but the test remains.
sendingOnABrokenConnectionDoesNotHang :: IO ()
sendingOnABrokenConnectionDoesNotHang = do
  hpgsqlConnInfo <- testConnInfo
  waitOutcomeVar <- newEmptyMVar
  -- The dying connection's own cleanup (on withConnection's exit) also tries to write to the
  -- now-broken socket and throws some other exception; that's expected and not what this test
  -- is checking, so it's discarded here rather than conflated with waitOutcomeVar's content.
  _ <- try @_ @SomeException $ withConnection hpgsqlConnInfo 10 $ \conn -> do
    let pid = getBackendPid conn
    -- Wait (up to 5s) for the backend to have *fully* terminated before we try to use `conn`
    -- again, so its socket is already broken by the time we attempt to send on it below.
    -- Note that with the original bug, this test actually went on to consume CPU forever
    -- because the send loop was uninterruptible.
    withConnection hpgsqlConnInfo 10 $ \killerConn -> do
      Only terminated <- query1 killerConn (mkQuery "SELECT pg_terminate_backend($1, 5000)" (Only pid))
      terminated `shouldBe` True
    waitOutcome <- timeout 5_000_000 (try @_ @IrrecoverableHpgsqlError (execute_ conn "SELECT 1"))
    putMVar waitOutcomeVar waitOutcome
  waitOutcome <- readMVar waitOutcomeVar
  case waitOutcome of
    Nothing -> expectationFailure "A query sent on a connection whose backend was already terminated hung instead of throwing"
    Just (Right ()) -> expectationFailure "A query succeeded on a connection whose backend was already terminated"
    Just (Left ex) -> ex `shouldSatisfy` irrecoverableErrorWithMsg "Remote peer has closed the connection"
