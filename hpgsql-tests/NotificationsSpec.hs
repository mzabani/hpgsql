module NotificationsSpec where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsg,
    testConnInfo,
  )
import Hpgsql
import Hpgsql.Notification (NotificationResponse (..), getNotification, getNotificationNonBlocking)
import Hpgsql.Pool (beforeReturningToPool)
import Hpgsql.Query (sql)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  aroundConn $ describe "Notifications" $ do
    it
      "Send notifications and then receive them"
      sendNotifAndReceiveIt

    it
      "Mixing order of sending and receiving notifications"
      mixedOrderSendAndReceiveNotifications

    -- Read the comment inside the test below to understand why
    -- it deadlocks. I don't think we can do something about this
    -- unless we can detect one thread is blocked on the other.
    -- We probably shouldn't try anything of the sort, though.
    xit
      "getNotification blocks and works even after orphaned query"
      getNotificationBlocksAndWorksEvenAfterOrphanedQuery
  aroundConn $ describe "beforeReturningToPool" $ do
    it
      "Pending notification is cleared by beforeReturningToPool"
      pendingNotifClearedByBeforeReturningToPool
    it
      "Bad transaction state means exception"
      badTransactionStateMeansException

sendNotifAndReceiveIt :: HPgConnection -> IO ()
sendNotifAndReceiveIt conn = do
  let currentPid = getBackendPid conn
  execute_ conn "LISTEN test_chan"
  execute_ conn "NOTIFY test_chan"
  execute_ conn "NOTIFY test_chan, 'hello'"
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "test_chan", notifPayload = ""}
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "test_chan", notifPayload = "hello"}
  execute_ conn "NOTIFY test_chan, 'hello again'"
  execute_ conn "SELECT 91" -- Ensure NotificationResponse can be received at any time
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "test_chan", notifPayload = "hello again"}

mixedOrderSendAndReceiveNotifications :: HPgConnection -> IO ()
mixedOrderSendAndReceiveNotifications conn = do
  let currentPid = getBackendPid conn
  execute_ conn "LISTEN \"tést_chãn\""
  execute_ conn "NOTIFY \"tést_chãn\", 'hello'"
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "tést_chãn", notifPayload = "hello"}
  withAsync (getNotification conn) $ \asyncNotif -> do
    threadDelay 100_000
    -- We have to use a different connection because our
    -- implementation isn't smart enough to keep the pipeline
    -- free for sending queries while `getNotification` is blocked.
    hpgsqlConnInfo <- testConnInfo
    withConnection hpgsqlConnInfo 10 $ \conn2 -> do
      execute_ conn2 "NOTIFY \"tést_chãn\", 'hello again'"
      wait asyncNotif `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "tést_chãn", notifPayload = "hello again"}
      execute_ conn2 "NOTIFY \"tést_chãn\", 'hello agáin 2'"
      execute_ conn2 "NOTIFY \"tést_chãn\", 'hello again 3'"
      execute_ conn "NOTIFY \"tést_chãn\", 'hello again ç 4'"
      getNotification conn `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "tést_chãn", notifPayload = "hello agáin 2"}
      getNotification conn `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "tést_chãn", notifPayload = "hello again 3"}
    getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "tést_chãn", notifPayload = "hello again ç 4"}
    getNotificationNonBlocking conn `shouldReturn` Nothing

getNotificationBlocksAndWorksEvenAfterOrphanedQuery :: HPgConnection -> IO ()
getNotificationBlocksAndWorksEvenAfterOrphanedQuery conn = do
  -- This example deadlocks, and I'm not sure it can be avoided.
  -- By running `getNotification` in a separate thread and then
  -- `wait notifAsync`, `getNotification` will block until the
  -- pipeline is consumed by its original thread or until that
  -- thread dies. But that thread will never die and never consume
  -- query results (or fire another query) because it's blocked on
  -- `wait notifAsync`.
  mainTid <- myThreadId
  execute_ conn "LISTEN test_chan"
  timeout 100_000 (execute_ conn "SELECT pg_sleep(10)") `shouldReturn` Nothing
  putStrLn "A"
  withAsync
    ( do
        getNotifThreadId <- myThreadId
        putStrLn $ "getNotification threadId: " ++ show getNotifThreadId
        getNotification conn
    )
    $ \notifAsync -> do
      secondTid <- myThreadId
      print $ "Main and second thread ids: " ++ show (mainTid, secondTid)
      putStrLn "B"
      hpgsqlConnInfo <- testConnInfo
      putStrLn "C"
      withConnection hpgsqlConnInfo 10 $ \conn2 -> do
        putStrLn "D"
        execute_ conn2 "NOTIFY test_chan, 'hey'"
        putStrLn "E"
        wait notifAsync `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "test_chan", notifPayload = "hey"}

pendingNotifClearedByBeforeReturningToPool :: HPgConnection -> IO ()
pendingNotifClearedByBeforeReturningToPool conn = do
  execute_ conn "LISTEN test_chan"
  execute_ conn "NOTIFY test_chan, 'hello'"
  beforeReturningToPool conn Nothing
  -- 100ms should be plenty of time to receive a notification if there was one
  timeout 100_000 (getNotification conn) `shouldReturn` Nothing

badTransactionStateMeansException :: HPgConnection -> IO ()
badTransactionStateMeansException conn = do
  execute_ conn "BEGIN"
  beforeReturningToPool conn Nothing `shouldThrow` irrecoverableErrorWithMsg "transaction was left in an invalid state"
