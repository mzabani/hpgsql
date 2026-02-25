module HPgsql
  ( query,
    queryWith,
    queryWithStreaming,
    putCopyData,
    withConnection,
    withConnectionOpts,
    cancelAnyRunningStatement,
    ConnString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    RowParser (..),
    HPgConnection, -- No ctor exposed
    Query, -- No ctor exposed
    TransactionStatus (..),
    Only (..),
    FromPgRow (..),
    FromPgField (..),
    NotificationResponse (..),
    Pipeline, -- Don't expose Pipeline's constructor!
    PoolCleanup (..),
    beforeReturningToPool,
    connect,
    connectOpts,
    defaultConnectOpts,
    closeGracefully,
    closeForcefully,
    getParameterStatus,
    execute,
    execute_,
    executeMany_,
    executeMany,
    connectionTransactionStatus,
    mkQuery,
    runPipeline,
    withCopy,
    pipelineCmd,
    pipelineL,
    pipelineS,
    getBackendPid,
    getNotification,
    getNotificationNonBlocking,
  )
where

import Control.Applicative (Alternative (..))
import Control.Concurrent (ThreadId, mkWeakThreadId, modifyMVar, myThreadId, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM, TQueue, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (Exception (..), MonadThrow, SomeException, bracket, bracketOnError, finally, mask, mask_, onException, throw, try, withException)
import Control.Monad (forM, forM_, join, unless, void, when)
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.Attoparsec.ByteString.Lazy as LazyParsec
import Data.Bifunctor (second)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import Data.Time (DiffTime, diffTimeToPicoseconds, secondsToDiffTime)
import Data.Word (Word64)
import GHC.Conc.Sync (ThreadStatus (..), fromThreadId, threadStatus)
import HPgsql.Connection (ConnString (..))
import HPgsql.Field (FromPgField (..), FromPgRow (..), Only (..), RowParser (..), ToPgRow (..))
import HPgsql.Msgs (AuthenticationOk, BackendKeyData (..), Bind (..), BindComplete, CancelRequest (..), CommandComplete (..), CopyData (..), CopyDone (..), CopyInResponse, DataRow (..), Describe (..), ErrorDetail (..), ErrorResponse (..), Execute (..), FromPgMessage (..), NoData, NoticeResponse (..), NotificationResponse (..), ParameterStatus (..), Parse (..), ParseComplete (..), PgMsgParser (..), ReadyForQuery (..), RowDescription (..), StartupMessage (..), Sync (..), Terminate (..), ToPgMessage (..), TransactionStatus (..), parsePgMessage)
import qualified HPgsql.Msgs as Msgs
import HPgsql.Query (Query (..), SingleQuery (..))
import HPgsql.TypeInfo (Format (..))
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as SocketLBS
import Streaming (Of (..), Stream)
import qualified Streaming as S
import qualified Streaming.Internal as SInternal
import qualified Streaming.Prelude as S
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak, deRefWeak)
import System.Timeout (timeout)

data HPgConnection = HPgConnection
  { socket :: !Socket,
    -- | We use STM transactions to protect the buffer
    -- against concurrent access, so maybe this MVar could be
    -- an IORef? And all the other MVars, for that matter.
    recvBuffer :: !(MVar LBS.ByteString),
    -- | A buffer of messages to be sent and of internal state
    -- transactions to commit when they are sent.
    sendBuffer :: !(MVar [(LBS.ByteString, STM ())]),
    originalConnStr :: !ConnString,
    parameterStatusMap :: !(MVar (Map Text Text)),
    internalConnectionState :: !(TVar InternalConnectionState),
    connPid :: !Int32,
    cancelSecretKey :: !Int32,
    connOpts :: !ConnectOpts
  }

instance Eq HPgConnection where
  conn1 == conn2 = socket conn1 == socket conn2

data InternalConnectionState = InternalConnectionState
  { totalQueriesSent :: !Integer,
    -- | We store the ThreadId holding the lock and an Int representing how many
    -- nested lock grabs it took so we can nest `withControlMsgsLock` in the same
    -- thread without running into deadlocks. This might no longer be useful after
    -- some refactors, but it generally allows us not to worry about function
    -- composition.
    blockedForSendingOrReceivingMsgsAtomically :: !(Maybe (Word64, Int)),
    -- | We support only one pipeline sent to the backend at a time
    -- (i.e. we don't send any messages to the backend after a `Sync` and until all query results
    -- are fully consumed, or while there's a COPY statement running) because otherwise we'd not
    -- be able to reliably send a cancellation request to the backend.
    -- Imagine we've sent messages for many pipelines, a user thread is killed waiting for results,
    -- and we send a cancellation request. By the time it arrives, the backend might be processing
    -- queries from the second pipeline, which we didn't want to cancel.
    currentPipeline :: ![QueryState],
    lastReadyForQueryReceived :: !ReadyForQuery,
    fundamentalCommunicationError :: !(Maybe IrrecoverableHpgsqlError),
    notificationsReceived :: !(TQueue Msgs.NotificationResponse)
  }

data CopyQueryState = StillCopying | CopyDoneAndSyncSent | CopyFailAndSyncSent
  deriving stock (Eq, Show)

data QueryProtocol
  = CopyQuery CopyQueryState
  | ExtendedQuery
  deriving stock (Eq, Show)

data ResponseMsg = RespParseComplete ParseComplete | RespBindComplete BindComplete | RespNoData NoData | RespRowDescription RowDescription | RespCopyInResponse CopyInResponse | RespErrorResponse ErrorResponse | RespCommandComplete CommandComplete | RespDataRow DataRow | RespReadyForQuery ReadyForQuery
  deriving stock (Show)

data ResponseMsgsReceived = NoMsgsReceived | ParseCompleteReceived ParseComplete | BindCompleteReceived BindComplete | RowDescriptionOrNoDataOrCopyInResponseReceived (Either3 NoData RowDescription CopyInResponse) | ErrorResponseReceived (Maybe (Either3 NoData RowDescription CopyInResponse)) ErrorResponse | CommandCompleteReceived (Either3 NoData RowDescription CopyInResponse) CommandComplete | ReadyForQueryReceived (Either ErrorResponse CommandComplete) ReadyForQuery
  deriving stock (Show)

data WeakThreadId = WeakThreadId (Weak ThreadId) Word64

instance Eq WeakThreadId where
  WeakThreadId _ t1 == WeakThreadId _ t2 = t1 == t2

instance Ord WeakThreadId where
  compare (WeakThreadId _ t1) (WeakThreadId _ t2) = compare t1 t2

instance Show WeakThreadId where
  show (WeakThreadId _ threadIdentifier) = "WeakThreadId " ++ show threadIdentifier

data QueryState = QueryState
  { queryIdentifier :: !QueryId,
    queryProtocol :: !QueryProtocol,
    queryOwner :: !(Maybe WeakThreadId),
    -- | Storing every single "control" (i.e. not `DataRow`) message received for each query
    -- means we can continue to drain its results from another thread at anytime, which is
    -- necessary to continue using the connection in the presence of asynchronous exceptions.
    -- We sadly cannot create a forkIO'd version of `atomicallyReceiveMsgs` because a query
    -- like `pg_sleep(999)` will make postgres not send even the first `ParseComplete` message
    -- for the sleep duration, meaning our internal lock would last that long and we wouldn't
    -- be able to send a CancelMsg. We could make our locking more granular, but that seems
    -- even more like a recipe for disaster.
    responseMsgsState :: ResponseMsgsReceived
  }
  deriving stock (Show)

-- | An Integer avoids any headaches from wrap-around when comparing query ids.
-- An Int64 should be fine, but the cost of this is negligible.
newtype QueryId = QueryId Integer
  deriving newtype (Enum, Eq, Num, Ord, Show)

connectionTransactionStatus :: HPgConnection -> IO TransactionStatus
connectionTransactionStatus conn = STM.atomically $ updateConnStateTxn conn $ \sttv -> do
  st <- STM.readTVar sttv
  pure $ case currentPipeline st of
    [] -> (\(ReadyForQuery s) -> s) $ lastReadyForQueryReceived st -- Remember we may or may not be inside a transaction
    qs
      | any
          ( ( \case
                ErrorResponseReceived _ _ -> True
                _ -> False
            )
              . responseMsgsState
          )
          qs ->
          TransInError
      | otherwise -> TransInTrans

data ConnectOpts = ConnectOpts
  { -- | How long in ms HPgsql will sleep before re-checking if active queries have been orphaned
    -- from their issuing threads having died. The default is 500ms, and this is only relevant
    -- if you plan on concurrently issuing queries on a single connection, and even then only
    -- if you expect your threads to be killed by asynchronous exceptions frequently enough,
    -- and you want resume using the connection and cannot wait ~500ms until HPgsql realizes
    -- it's fine to do so.
    -- You probably don't need to worry about this or tune it.
    killedThreadPollIntervalMs :: Int
  }

connect :: ConnString -> DiffTime -> IO HPgConnection
connect =
  connectOpts defaultConnectOpts

connectOpts :: ConnectOpts -> ConnString -> DiffTime -> IO HPgConnection
connectOpts connOpts =
  internalConnectOrCancel
    Connect
    connOpts

defaultConnectOpts :: ConnectOpts
defaultConnectOpts =
  ConnectOpts
    { killedThreadPollIntervalMs = 500
    }

data InternalConnectOrCancelRequest a where
  Connect :: InternalConnectOrCancelRequest HPgConnection
  CancelNotConnect :: CancelRequest -> InternalConnectOrCancelRequest ()

internalConnectOrCancel :: InternalConnectOrCancelRequest a -> ConnectOpts -> ConnString -> DiffTime -> IO a
internalConnectOrCancel connectOrCancel connOpts originalConnStr@ConnString {..} conntimeout = do
  -- TODO: Proper exception rethrowing when we fail to connect
  sockOrTimeout <- timeout (fromInteger $ diffTimeToPicoseconds conntimeout `div` 1_000_000) $ getConnectedSocket Nothing
  case sockOrTimeout of
    Nothing -> throwIrrecoverableError "Could not connect in the supplied timeout"
    -- TODO: It's still possible for an asynchronous exception to interrupt this before the `onException` handler is installed
    Just sock -> flip onException (Socket.close sock) $ do
      recvBuffer <- newMVar mempty
      sendBuffer <- newMVar mempty
      connParams <- newMVar mempty
      notifQueue <- STM.newTQueueIO
      currentConnectionState <-
        STM.newTVarIO $
          InternalConnectionState
            { totalQueriesSent = 0,
              blockedForSendingOrReceivingMsgsAtomically = Nothing,
              currentPipeline = [],
              lastReadyForQueryReceived = ReadyForQuery TransIdle,
              fundamentalCommunicationError = Nothing,
              notificationsReceived = notifQueue
            }
      let hpgConnPartialDoNotReturn = HPgConnection sock recvBuffer sendBuffer originalConnStr connParams currentConnectionState 0 0 connOpts
      case connectOrCancel of
        CancelNotConnect cancelRequest -> do
          interruptibleSendMsg hpgConnPartialDoNotReturn cancelRequest
          Socket.close sock
        Connect -> do
          -- TODO: Send encoding and other things with "options"?
          interruptibleSendMsg hpgConnPartialDoNotReturn $ StartupMessage {user, database, options}
          void $ receiveNextMsgUnsafe hpgConnPartialDoNotReturn (msgParser @AuthenticationOk)
          errorOrBackendKeyData <- receiveNextMsgUnsafe hpgConnPartialDoNotReturn $ Right <$> msgParser @BackendKeyData <|> Left <$> msgParser @ErrorResponse
          -- TODO: Throw informative error for unimplemented authentication methods
          case errorOrBackendKeyData of
            Left (ErrorResponse errDetails) -> throw $ IrrecoverableHpgsqlError {hpgsqlDetails = "Socket connected but postgresql threw an error during connection startup handshake", pgErrorDetails = errDetails}
            Right backendKeyData -> do
              readyForQueryOrError <- receiveNextMsgUnsafe hpgConnPartialDoNotReturn $ Right <$> msgParser @ReadyForQuery <|> Left <$> msgParser @ErrorResponse
              case readyForQueryOrError of
                Left (ErrorResponse errDetails) -> throw $ IrrecoverableHpgsqlError {hpgsqlDetails = "Some postgresql error happened while connecting", pgErrorDetails = errDetails}
                Right rq@ReadyForQuery {} -> STM.atomically $ do
                  st <- STM.readTVar currentConnectionState
                  STM.writeTVar currentConnectionState $ st {lastReadyForQueryReceived = rq}
              debugPrint $ "Connected with backend PID " ++ show (backendPid backendKeyData)
              pure $ hpgConnPartialDoNotReturn {connPid = backendPid backendKeyData, cancelSecretKey = backendSecretKey backendKeyData}
  where
    -- \| Unsafe version of `withSafeReceiveNextMsg`.
    receiveNextMsgUnsafe :: (Show a) => HPgConnection -> PgMsgParser a -> IO a
    receiveNextMsgUnsafe conn parser = receiveNextMsgWithMaskedContinuation conn parser pure

    -- TODO: Get ParameterStatus and set client_encoding to UTF8 if it isn't
    getConnectedSocket resolvedAddr = do
      addrInfo <- case resolvedAddr of
        Just addrInfo -> pure addrInfo
        Nothing -> do
          addrInfos <- Socket.getAddrInfo (Just Socket.defaultHints) (Just hostname) (Just $ show port)
          case addrInfos of
            [] -> throwIrrecoverableError "Could not resolve address"
            addrInfo : _ -> pure addrInfo
      connattempt <- try $ do
        sock <- Socket.openSocket addrInfo
        Socket.connect sock (Socket.addrAddress addrInfo)
        pure sock
      case connattempt of
        Left (_ :: SomeException) -> getConnectedSocket (Just addrInfo)
        Right s -> pure s

getParameterStatus :: HPgConnection -> Text -> IO (Maybe Text)
getParameterStatus HPgConnection {parameterStatusMap} paramName = Map.lookup paramName <$> readMVar parameterStatusMap

getBackendPid :: HPgConnection -> Int32
getBackendPid HPgConnection {connPid} = connPid

data PoolCleanup = PoolCleanup
  { -- | Runs `RESET ALL` and `RESET ROLE` on the connection. Defaults to True.
    resetAll :: Bool,
    -- | Runs `UNLISTEN *` on the connection and clears the internal queue of notifications. Defaults to True.
    unlistenAll :: Bool,
    -- | Throws an exception if there is an open transaction or if there's a transaction in error state. Defaults to True.
    checkTransactionState :: Bool
    -- TODO: Check for any temporary tables and throw?
  }

-- | Run this function before putting the connection back into a pool
-- to run some health checks and reset some forms of connection state.
beforeReturningToPool ::
  HPgConnection ->
  -- | Pass `Nothing` to use defaults.
  Maybe PoolCleanup ->
  IO ()
beforeReturningToPool conn@HPgConnection {internalConnectionState} mCleanOpts = do
  STM.atomically $ do
    st <- STM.readTVar internalConnectionState
    unless (null $ currentPipeline st) $ throwIrrecoverableError "There are still active queries in progress. Make sure to close this connection with `closeForcefully` or consume all existing queries' results"
  when (checkTransactionState cleanOpts) $ do
    txnStatus <- connectionTransactionStatus conn
    unless (txnStatus == TransIdle) $ throwIrrecoverableError $ "The connection's transaction was left in an invalid state: " ++ show txnStatus ++ ". Make sure to close this connection with `closeForcefully`"
  -- What if there are notifications in the socket buffer? It seems reasonable to assume that when
  -- running "UNLISTEN *" those would be received, so this might be fine as long as we
  -- clear the internal queue _after_ "UNLISTEN *".
  executeMany_ conn $ (if (resetAll cleanOpts) then ["RESET ALL", "RESET ROLE"] else []) ++ (if unlistenAll cleanOpts then ["UNLISTEN *"] else [])
  when (unlistenAll cleanOpts) clearInternalNotificationQueue
  where
    cleanOpts = fromMaybe PoolCleanup {resetAll = True, unlistenAll = True, checkTransactionState = True} mCleanOpts
    clearInternalNotificationQueue = STM.atomically $ do
      st <- STM.readTVar internalConnectionState
      emptyQueue <- STM.newTQueue
      STM.writeTVar internalConnectionState $ st {notificationsReceived = emptyQueue}

-- | Closes the connection with postgres. Do not use this in exception handlers; use `closeForcefully`
-- instead.
closeGracefully :: HPgConnection -> IO ()
closeGracefully conn@(HPgConnection {socket}) = flip finally (Socket.close socket) $ do
  withControlMsgsLock
    conn
    (const $ pure ())
    -- When done closing, put an error in case any orphaned threads from the user still
    -- try to consume the connection
    ( \sttv -> do
        st <- STM.readTVar sttv
        STM.writeTVar sttv $ st {fundamentalCommunicationError = Just $ IrrecoverableHpgsqlError {hpgsqlDetails = "Bug in HPgsql. Should not be in this state when receiving control msgs because firstPendingQry should have thrown in this case", pgErrorDetails = mempty}}
    )
    $ \() -> do
      interruptibleSendMsg conn Terminate

-- | Closes the connection with postgres as quickly as possible without
-- the proper postgres protocol handshake procedures. This is equivalent to
-- closing the connection's socket in the kernel without making postgres
-- aware of it.
-- Use this if you need to close the connection in exception handlers or
-- if you received an irrecoverable HPgsql exception.
closeForcefully :: HPgConnection -> IO ()
closeForcefully (HPgConnection {socket}) = Socket.close socket

withConnection :: ConnString -> DiffTime -> (HPgConnection -> IO a) -> IO a
withConnection connstr conntimeout f = bracketOnError (connect connstr conntimeout) closeForcefully $ \conn -> do
  res <- f conn
  closeGracefully conn
  pure res

withConnectionOpts :: ConnectOpts -> ConnString -> DiffTime -> (HPgConnection -> IO a) -> IO a
withConnectionOpts connOpts connstr conntimeout f = bracketOnError (connectOpts connOpts connstr conntimeout) closeForcefully $ \conn -> do
  res <- f conn
  closeGracefully conn
  pure res

-- | Just like `receiveNextMsgWithMaskedContinuation` but passes a `Maybe a` to
-- the continuation instead of throwing an exception on parser failure. On parsing
-- failure, this makes sure the message buffer remains unaltered.
receiveNextMsgWithMaskedContinuation :: (Show a) => HPgConnection -> PgMsgParser a -> (a -> IO b) -> IO b
receiveNextMsgWithMaskedContinuation conn parser f =
  receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure conn parser $ \case
    Right p -> f p
    Left msgIdentChar -> throwIrrecoverableError $ "Could not parse postgres message with ident char " ++ show msgIdentChar ++ ". This is an internal error in HPGsql. Please report it."

-- | Masks asynchronous exceptions in between the moment the message is extracted from
-- the internal buffer and the supplied function runs to completion.
-- This is important for control messages (i.e. not `DataRow`) because if you extract a
-- message from the buffer, you must be able to update the connection's internal state,
-- lest it will be left in a very broken place.
-- CAREFUL: avoid doing networking or too much work in your supplied function. It must
-- be really cheap!
receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure :: (Show a) => HPgConnection -> PgMsgParser a -> (Either Char a -> IO b) -> IO b
receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure conn@HPgConnection {socket, recvBuffer} parser f = do
  -- We need to preserve the invariant that the internal buffer's first byte is
  -- always the first byte of a valid Message.
  -- This means we can't extract a message partially from the internal buffer,
  -- even in face of asynchronous exceptions.
  -- So we append to the buffer up until the time to extract a message from the buffer,
  -- at which point we use masking.
  receiveUntilBufferHasAtLeast 5
  charAndLength <- peekIntoBuffer 5
  let (w2c -> msgIdentChar, lenbs) = fromMaybe (error "impossible") $ LBS.uncons charAndLength
      lenLeftToFetch :: Int64 = fromIntegral $ Binary.decode @Int32 lenbs - 4
      fullMessageLen = 5 + lenLeftToFetch
  receiveUntilBufferHasAtLeast fullMessageLen
  receivedNoticeOrParameterSoTryAgain <- go msgIdentChar fullMessageLen
  -- We don't let `go` recursively call itself or even `receivedNoticeOrParameterSoTryAgain`
  -- because it would inherit its own masking, which would be bad!
  -- We don't want to have to test the behaviour of our functions with both
  -- masking and unmasking!
  case receivedNoticeOrParameterSoTryAgain of
    Nothing -> receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure conn parser f
    Just res -> pure res
  where
    go msgIdentChar fullMessageLen = mask_ $ do
      restOfMsg <- LBS.drop 5 <$> peekIntoBuffer fullMessageLen
      case parsePgMessage msgIdentChar restOfMsg parser of
        Just msg -> do
          removeFirstBytesFromBufferOrThrow fullMessageLen
          debugPrint $ "Received " ++ show msg
          Just <$> f (Right msg)
        Nothing -> do
          -- This could be a Notification, NOTICE or a ParameterStatus message, since these
          -- can be received _at any time_ according to the docs.
          case parsePgMessage msgIdentChar restOfMsg (Left3 <$> msgParser @NotificationResponse <|> Middle3 <$> msgParser @NoticeResponse <|> Right3 <$> msgParser @ParameterStatus) of
            Just (Left3 notifResponse) -> do
              debugPrint "Received notification. Will add it to internal queue."
              removeFirstBytesFromBufferOrThrow fullMessageLen
              STM.atomically $ do
                sttv <- STM.readTVar $ internalConnectionState conn
                STM.writeTQueue (notificationsReceived sttv) notifResponse
              pure Nothing
            Just (Middle3 (NoticeResponse details)) -> do
              removeFirstBytesFromBufferOrThrow fullMessageLen
              -- TODO: Print to stderr (what does psql do?)? Print in a nicer format?
              let severity =
                    fromMaybe
                      "Notice of unknown severity"
                      (Map.lookup ErrorSeverity details)
                  humanmsg = fromMaybe "no message" (Map.lookup ErrorHumanReadableMsg details)
              Text.putStrLn $ decodeUtf8 $ LBS.toStrict $ severity <> ": " <> humanmsg
              pure Nothing
            Just (Right3 (ParameterStatus {..})) -> do
              removeFirstBytesFromBufferOrThrow fullMessageLen
              modifyMVar_ (parameterStatusMap conn) $ \paramMap -> pure $ Map.insert parameterName parameterValue paramMap
              pure Nothing
            Nothing -> Just <$> f (Left msgIdentChar)

    removeFirstBytesFromBufferOrThrow :: Int64 -> IO ()
    removeFirstBytesFromBufferOrThrow nbytes =
      modifyMVar_ recvBuffer $ \lbs -> do
        if LBS.length lbs >= nbytes
          then pure $ LBS.drop nbytes lbs
          else
            error "Bug in HPgsql. Internal buffer's bytes weren't filled enough"

    socketRecvAtLeast :: Int64 -> IO LBS.ByteString
    socketRecvAtLeast n = do
      l1 <- SocketLBS.recv socket (max n 8192) -- Do we need `max`?
      if LBS.length l1 >= n
        then pure l1
        else do
          rest <- socketRecvAtLeast (n - LBS.length l1)
          pure $ l1 <> rest

    -- \| Appends into the internal buffer by reading from the socket
    -- until the buffer has at least N bytes.
    receiveUntilBufferHasAtLeast :: Int64 -> IO ()
    receiveUntilBufferHasAtLeast minBytesNecessary = do
      nBytesInBuffer <- LBS.length <$> readMVar recvBuffer
      when (nBytesInBuffer < minBytesNecessary) $ mask $ \restore -> do
        -- This takes from the kernel's recv buffer and appendds to our buffer atomically,
        -- or an exception is thrown when receiving
        restOfBytes <- restore $ socketRecvAtLeast (minBytesNecessary - nBytesInBuffer)
        modifyMVar_ recvBuffer (\lbs -> pure $ lbs <> restOfBytes)

    peekIntoBuffer :: Int64 -> IO LBS.ByteString
    peekIntoBuffer n = LBS.take n <$> readMVar recvBuffer

-- | Cancels any running statements in the current connection, including COPY, or returns if there
-- is no active query to cancel.
-- Make sure you do not try to consume results of queries you have already sent if you run this, or
-- behaviour is undefined. That means if you had a Stream result and you run this function, you should
-- not further inspect the Stream, and if you had sent a pipeline with multiple queries and you run this
-- function, you should not try to consume the results of any query in that pipeline.
cancelAnyRunningStatement :: HPgConnection -> IO ()
cancelAnyRunningStatement conn = do
  copyState <-
    STM.atomically $
      STM.readTVar (internalConnectionState conn) >>= \st -> pure $ case currentPipeline st of
        [QueryState {queryProtocol = CopyQuery copyState}] -> Just copyState
        _ -> Nothing
  let markCopyFailSent = do
        let sttv = internalConnectionState conn
        st <- STM.readTVar sttv
        case currentPipeline st of
          [qs@QueryState {queryProtocol = CopyQuery StillCopying}] -> STM.writeTVar (internalConnectionState conn) $ st {currentPipeline = [qs {queryProtocol = CopyQuery CopyFailAndSyncSent}]}
          _ -> throwIrrecoverableError "Impossible: when marking CopyFail state was invalid"
  case copyState of
    Just StillCopying ->
      uninterruptibleSendControlMsgs_ conn ([SomeMessage $ Msgs.CopyFail "COPY statement automatically cancelled by HPgsql because it was interrupted", SomeMessage Sync], markCopyFailSent)
    Just CopyDoneAndSyncSent ->
      pure
        () -- Already finished, nothing to cancel
    Just CopyFailAndSyncSent ->
      pure
        () -- Already cancelled, no need to send another
    Nothing ->
      internalConnectOrCancel
        (CancelNotConnect $ CancelRequest (connPid conn) (cancelSecretKey conn))
        (connOpts conn)
        (originalConnStr conn)
        (secondsToDiffTime 30)

isLastInPipeline :: HPgConnection -> QueryId -> IO Bool
isLastInPipeline conn qryId = STM.atomically $ updateConnStateTxn conn $ \sttv -> do
  InternalConnectionState {currentPipeline = queries} <- STM.readTVar sttv
  pure $ case lastMaybe queries of
    Nothing -> False
    Just q -> queryIdentifier q == qryId

updateConnStateTxn :: HPgConnection -> (TVar InternalConnectionState -> STM a) -> STM a
updateConnStateTxn conn f = do
  st <- STM.readTVar $ internalConnectionState conn
  whenJust (fundamentalCommunicationError st) throw
  f (internalConnectionState conn)

withControlMsgsLock ::
  HPgConnection ->
  -- | Applied in the same STM transaction that takes the lock
  (TVar InternalConnectionState -> STM a) ->
  -- | Applied in the same STM transaction that releases the lock
  (TVar InternalConnectionState -> STM b) ->
  (a -> IO c) ->
  IO c
withControlMsgsLock conn acqStm relStm f = do
  -- To make sure any messages sent by threads killed before
  -- we even look a internal connection state have their effects
  -- on internal state, we flush the send buffer.
  flushSendBuffer
  thisThreadId <- fromThreadId <$> myThreadId
  bracket
    ( do
        acq <- STM.atomically $ updateConnStateTxn conn $ \sttv -> do
          st <- STM.readTVar sttv
          let blockedBy = blockedForSendingOrReceivingMsgsAtomically st
          newSt <- case blockedBy of
            Nothing -> pure $ st {blockedForSendingOrReceivingMsgsAtomically {- traceShowWith ("Grabbing ",) $ -} = Just (thisThreadId, 1)}
            Just (tid, nGrabs) ->
              if tid == thisThreadId then pure $ st {blockedForSendingOrReceivingMsgsAtomically {- traceShowWith ("Grabbing ",) $ -} = Just (thisThreadId, nGrabs + 1)} else STM.retry
          STM.writeTVar sttv newSt
          acqStm sttv
        -- debugPrint "Internal state: [Acquired control-msg-lock]."
        pure acq
    )
    -- Release lock on success or error
    ( const $ do
        void $ STM.atomically $ updateConnStateTxn conn $ \sttv -> do
          st <- STM.readTVar sttv
          let blockedBy = blockedForSendingOrReceivingMsgsAtomically st
          newSt <- case blockedBy of
            Nothing -> throwIrrecoverableError "Impossible: should have been blocked but was not!"
            Just (tid, nGrabs) ->
              let newLockState = {- traceShowWith ("Releasing ",) $ -} if nGrabs <= 1 then Nothing else Just (thisThreadId, nGrabs - 1)
               in if tid == thisThreadId then pure st {blockedForSendingOrReceivingMsgsAtomically = newLockState} else throwIrrecoverableError "Impossible: Lock of a different thread!"
          STM.writeTVar sttv newSt
          relStm sttv
          -- debugPrint "Internal state: [Released control-msg-lock]."
    )
    ( \acq -> do
        res <- f acq
        -- Flush the send buffer that `f` may have populated and
        -- apply all internal connection state changes before we
        -- let the release STM transaction look at it.
        flushSendBuffer
        pure res
    )
  where
    flushSendBuffer :: IO ()
    flushSendBuffer = mask $ \restore -> do
      threadWasKilled <- newMVar False
      unkillableThread <- async
        $ flip
          withException
          -- An exception here could be a socket error or something
          -- that forces us to discard the connection
          ( \ex -> do
              debugPrint $ "!!!!!!!!!!!!! Got synchronous exception: " ++ show ex
              STM.atomically $ updateConnStateTxn conn $ \sttv -> do
                st <- STM.readTVar sttv
                STM.writeTVar (internalConnectionState conn) $ st {fundamentalCommunicationError = Just ex}
          )
        $ do
          let go = do
                -- We run this in a forked thread to keep
                -- the user thread interruptible.
                others <- modifyMVar (sendBuffer conn) $ \case
                  [] -> pure ([], [])
                  ((msgs, afterSentTxn) : xs) ->
                    if LBS.null msgs
                      then do
                        -- debugPrint "Finished sending msgs"
                        STM.atomically afterSentTxn
                        pure (xs, xs)
                      else do
                        n <- SocketLBS.send (socket conn) msgs
                        -- debugPrint $ "Sent " ++ show n ++ ". Left: " ++ show (LBS.length (LBS.drop n msgs))
                        let fin = (LBS.drop n msgs, afterSentTxn) : xs
                        pure (fin, fin)

                stop <- readMVar threadWasKilled
                -- debugPrint $ show $ stop || null others
                unless (null others || stop) go
          go
      flip
        onException
        (modifyMVar_ threadWasKilled $ const $ pure True)
        $ restore
        $ wait unkillableThread

-- | Receives the next response message for the given QueryId safely/atomically, updates
-- internal connection state to reflect it, and returns the updated state alongside the
-- received message.
-- This also receives ReadyForQuery if the query is already in ErrorResponse or if it's the
-- last in the pipeline and has received CommandComplete.
-- You don't want to use this for receiving DataRows in large scale, because the costs of
-- STM transactions apply here.
receiveOutstandingResponseMsgsSafely :: WeakThreadId -> HPgConnection -> QueryId -> IO (Maybe ResponseMsg, ResponseMsgsReceived)
receiveOutstandingResponseMsgsSafely thisThreadId conn qryId = do
  -- debugPrint $ "Internal state: [Waiting] until ok to receive response control messages for QueryId " ++ show qryId
  withControlMsgsLock
    conn
    -- Check last received message and take lock before receiving next message
    ( \_sttv -> do
        queryState <- do
          blockUntilQueryTurn
          getQueryStateIfFirstOrThrow
        -- debugPrint $ "Internal state: [Acquired receive-lock]. QueryId " ++ show qryId
        pure queryState
    )
    (const $ pure ())
    $ \qryState -> do
      case responseMsgsState qryState of
        NoMsgsReceived -> receiveParseCompleteAtomically
        ParseCompleteReceived _ -> receiveBindCompleteAtomically
        BindCompleteReceived _ -> receiveNoDataOrRowDescriptionOrCopyInResponseAtomically
        RowDescriptionOrNoDataOrCopyInResponseReceived _ -> receiveDataRowOrCommandCompleteAtomically
        ErrorResponseReceived _ _ -> receiveReadyForQueryAtomically
        state@(CommandCompleteReceived _ _) -> ifM (isLastInPipeline conn qryId) receiveReadyForQueryAtomically (pure (Nothing, state)) -- Nothing more to receive after CommandComplete unless it's the last query in the pipeline
        state@(ReadyForQueryReceived _ _) -> pure (Nothing, state) -- Definitely nothing to receive if we're here
  where
    receiveParseCompleteAtomically = receiveNextMsgWithMaskedContinuation conn (Right <$> msgParser @ParseComplete <|> Left <$> msgParser @ErrorResponse) $ \msgE -> do
      STM.atomically $ updateQueryStateIfFirstOrThrow $ case msgE of
        Right msg -> RespParseComplete msg
        Left err -> RespErrorResponse err
    receiveBindCompleteAtomically = receiveNextMsgWithMaskedContinuation conn (Right <$> msgParser @BindComplete <|> Left <$> msgParser @ErrorResponse) $ \msgE -> do
      STM.atomically $ updateQueryStateIfFirstOrThrow $ case msgE of
        Right msg -> RespBindComplete msg
        Left err -> RespErrorResponse err
    receiveNoDataOrRowDescriptionOrCopyInResponseAtomically = receiveNextMsgWithMaskedContinuation conn (Right <$> (Right3 <$> msgParser @RowDescription <|> Left3 <$> msgParser @NoData <|> Middle3 <$> msgParser @CopyInResponse) <|> Left <$> msgParser @ErrorResponse) $ \msgE -> do
      STM.atomically $ updateQueryStateIfFirstOrThrow $ case msgE of
        Right (Left3 msg) -> RespNoData msg
        Right (Middle3 msg) -> RespCopyInResponse msg
        Right (Right3 msg) -> RespRowDescription msg
        Left err -> RespErrorResponse err
    receiveDataRowOrCommandCompleteAtomically = receiveNextMsgWithMaskedContinuation conn (Right3 <$> msgParser @DataRow <|> Middle3 <$> msgParser @CommandComplete <|> Left3 <$> msgParser @ErrorResponse) $ \msgE -> do
      STM.atomically $ updateQueryStateIfFirstOrThrow $ case msgE of
        Right3 msg -> RespDataRow msg
        Middle3 msg -> RespCommandComplete msg
        Left3 msg -> RespErrorResponse msg
    receiveReadyForQueryAtomically = receiveNextMsgWithMaskedContinuation conn (msgParser @ReadyForQuery) $ \rq -> STM.atomically $ updateQueryStateIfFirstOrThrow $ RespReadyForQuery rq
    getQueryStateIfFirstOrThrow :: STM QueryState
    getQueryStateIfFirstOrThrow = updateConnStateTxn conn $ \sttv -> do
      st <- STM.readTVar sttv
      case currentPipeline st of
        [] -> throwIrrecoverableError "Bug in HPgsql. No queries for getQueryStateIfFirstOrThrow"
        (splitQueries -> (earlierQueries, thisQuery))
          | not (all queryComplete earlierQueries) -> throwIrrecoverableError "Bug in HPgsql. Should not be receiving control messages for a query with the wrong turn"
          | otherwise -> pure thisQuery
    blockUntilQueryTurn :: STM ()
    blockUntilQueryTurn = updateConnStateTxn conn $ \sttv -> do
      st <- STM.readTVar sttv
      case currentPipeline st of
        [] ->
          -- This state can exist when trying to consume a query from a failed pipeline,
          -- since we clear that pipeline after ReadyForQuery.
          -- We assume this is not a bug (we have good enough test coverage, hopefully)
          -- and is that, instead.
          throwIrrecoverableError "Another query in the same pipeline threw an error, so this query did not even run, and therefore its results cannot be fetched."
        (splitQueries -> (earlierQueries, _))
          | any (\qs -> queryOwner qs == Just thisThreadId && not (queryComplete qs)) earlierQueries -> throwIrrecoverableError "HPgsql does not support consuming a query's results before consuming all previous queries' results from the same pipeline. This error can happen if you caught an IrrecoverableHpgsqlError earlier and decided to continue."
          | not (all queryComplete earlierQueries) -> STM.retry
          | otherwise -> pure ()

    splitQueries :: [QueryState] -> ([QueryState], QueryState)
    splitQueries qs = case List.span ((< qryId) . queryIdentifier) qs of
      (_, []) -> error "Could not find query with right Id"
      (as, firstQuery : _others)
        | queryIdentifier firstQuery == qryId -> (as, firstQuery)
        | otherwise -> error "Could not find query right Id (part 2)"

    queryComplete :: QueryState -> Bool
    queryComplete QueryState {responseMsgsState} = case responseMsgsState of
      CommandCompleteReceived _ _ -> True
      _ -> False

    updateQueryStateIfFirstOrThrow :: ResponseMsg -> STM (Maybe ResponseMsg, ResponseMsgsReceived)
    updateQueryStateIfFirstOrThrow respMsg = updateConnStateTxn conn $ \sttv -> do
      -- IMPORTANT: No `STM.retry` here as this is called unmasked and must terminate promptly!
      st <- STM.readTVar sttv
      firstPendingQry <- getQueryStateIfFirstOrThrow
      let newState = case (respMsg, responseMsgsState firstPendingQry) of
            (RespParseComplete msg, NoMsgsReceived) -> ParseCompleteReceived msg
            (RespBindComplete msg, ParseCompleteReceived _) -> BindCompleteReceived msg
            (RespNoData msg, BindCompleteReceived _) -> RowDescriptionOrNoDataOrCopyInResponseReceived $ Left3 msg
            (RespRowDescription msg, BindCompleteReceived _) -> RowDescriptionOrNoDataOrCopyInResponseReceived $ Middle3 msg
            (RespCopyInResponse msg, BindCompleteReceived _) -> RowDescriptionOrNoDataOrCopyInResponseReceived $ Right3 msg
            (RespDataRow _, RowDescriptionOrNoDataOrCopyInResponseReceived prev) -> RowDescriptionOrNoDataOrCopyInResponseReceived prev -- When draining a query that was already fetching rows, this can happen
            (RespErrorResponse msg, _) -> ErrorResponseReceived Nothing msg
            (RespCommandComplete msg, RowDescriptionOrNoDataOrCopyInResponseReceived noDataRowDesc) -> CommandCompleteReceived noDataRowDesc msg
            (RespReadyForQuery rq, ErrorResponseReceived _ err) -> ReadyForQueryReceived (Left err) rq
            (RespReadyForQuery rq, CommandCompleteReceived _ cmd) -> ReadyForQueryReceived (Right cmd) rq
            (_, before) -> error $ "Bug in HPgsql. Response messages in invalid order. Had " ++ show before ++ " and received " ++ show respMsg
          allQueries = currentPipeline st
      STM.writeTVar sttv $
        st
          { currentPipeline =
              -- Special handling: if we received `ReadyForQuery`, we empty the pipeline
              -- We do this here instead of when initiating a new pipeline for two reasons:
              -- 1. Querying transactionStatus after a query is consumed should reflect that new state (e.g. after sending "COMMIT", we should be in TransIdle), even if we do that before initiating a new pipeline.
              -- 2. It's one fewer state to worry about, as PendingResults [a, b, ..., { ReadyForQueryReceived }] is exactly the same as PendingResults [], for any purposes. Better update it as soon as possible.
              case newState of
                ReadyForQueryReceived _ _ -> []
                _ ->
                  map
                    ( \qs ->
                        if queryIdentifier qs == qryId
                          then
                            qs
                              { responseMsgsState = newState
                              }
                          else qs
                    )
                    allQueries,
            lastReadyForQueryReceived = case newState of
              ReadyForQueryReceived _ rq -> rq
              _ -> lastReadyForQueryReceived st
          }
      pure (Just respMsg, newState)

data Either3 a b c = Left3 a | Middle3 b | Right3 c
  deriving stock (Show)

-- | After sending one or more queries to the backend, run this function for each query to fetch that query's results.
-- You must call the returned IO function and consume the returned Stream completely until you get to the
-- `Either ErrorResponse CommandComplete` object.
-- For non-row returning statements like INSERT, DELETE, and UPDATE, the returned Stream will be empty but the execution status
-- will still be available in the Stream's result.
-- This function can also "consume results" of a `COPY FROM STDIN` statement, which essentially means it can receive the
-- control messages of that starting from any possible state.
-- By following these rules you will always keep the internal connection's state healthy, even in the presence of concurrency
-- and asynchronous exceptions.
consumeResults ::
  HPgConnection ->
  QueryId ->
  IO (Maybe (Either3 NoData RowDescription CopyInResponse), Stream (Of DataRow) IO (Either ErrorResponse CommandComplete))
consumeResults conn qryId = do
  -- debugPrint "++++ Inside consumeResults"
  -- We assume it's possible to receive a DataRow here even in the first call because `consumeResults`
  -- can be used to drain results or orphaned queries/pipelines that had been partially consumed.
  -- We could have two different functions - one for draining and another for consuming results -, but
  -- that's more code paths to test, with draining very rarely exercised.
  thisThreadId <- getMyWeakThreadId
  let receiveUntilTimeToReceiveRows :: IO (Maybe (Either3 NoData RowDescription CopyInResponse), Either3 ErrorResponse (Maybe DataRow) CommandComplete)
      receiveUntilTimeToReceiveRows = do
        nextMsg <- receiveOutstandingResponseMsgsSafely thisThreadId conn qryId
        case nextMsg of
          (Just (RespDataRow dr), RowDescriptionOrNoDataOrCopyInResponseReceived noDataOrRowDesc) -> pure (Just noDataOrRowDesc, Middle3 $ Just dr)
          (Just (RespDataRow _), _) -> throwIrrecoverableError "Impossible: Got DataRow but did not have RowDescOrNoData before?"
          (_, ErrorResponseReceived mNoDataOrRowDesc err) -> pure (mNoDataOrRowDesc, Left3 err)
          (_, CommandCompleteReceived noDataOrRowDesc cmd) -> pure (Just noDataOrRowDesc, Right3 cmd)
          (_, RowDescriptionOrNoDataOrCopyInResponseReceived noDataOrRowDesc) -> pure (Just noDataOrRowDesc, Middle3 Nothing)
          (_, ParseCompleteReceived _) -> receiveUntilTimeToReceiveRows
          (_, BindCompleteReceived _) -> receiveUntilTimeToReceiveRows
          (_, ReadyForQueryReceived errOrCmd _) -> pure (Nothing, either Left3 Right3 errOrCmd)
          (_, NoMsgsReceived) -> receiveUntilTimeToReceiveRows -- TODO: This should be unreachable
  firstMsg <- receiveUntilTimeToReceiveRows
  case firstMsg of
    (mERowDesc, Left3 err) -> do
      receiveReadyForQueryIfNecessary thisThreadId
      pure (mERowDesc, pure $ Left err)
    (mERowDesc, Right3 cmd) -> do
      receiveReadyForQueryIfNecessary thisThreadId
      pure (mERowDesc, pure $ Right cmd)
    (mERowDesc, Middle3 mDataRow) -> do
      let allOtherRows =
            S.unfold
              ( \() -> do
                  -- This is a bit ugly, but we try very carefully not to bear the cost of STM
                  -- transactions at all when receiving DataRows, since they can come in very large
                  -- amounts, but we need to make sure the _other_ important messages, like ErrorResponse
                  -- and CommandComplete, do update internal state.
                  mRow <- receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure conn (msgParser @DataRow) pure
                  case mRow of
                    Right row -> pure $ Right (row :> ())
                    Left _ -> do
                      stateAfterNextMsg <- snd <$> receiveOutstandingResponseMsgsSafely thisThreadId conn qryId
                      case stateAfterNextMsg of
                        ErrorResponseReceived _ err -> do
                          receiveReadyForQueryIfNecessary thisThreadId
                          pure $ Left $ Left err
                        CommandCompleteReceived _ cmd -> do
                          receiveReadyForQueryIfNecessary thisThreadId
                          pure $ Left $ Right cmd
                        ReadyForQueryReceived errOrCmd _ -> pure $ Left errOrCmd
                        _ -> throwIrrecoverableError "Internal error in HPgsql. After a DataRow we should get either an ErrorResponse or a CommandComplete message"
              )
              ()
          finalStream = case mDataRow of
            Nothing -> allOtherRows
            Just dr ->
              SInternal.Step $
                dr :> allOtherRows
      pure (mERowDesc, finalStream)
  where
    receiveReadyForQueryIfNecessary :: WeakThreadId -> IO ()
    receiveReadyForQueryIfNecessary thisThreadId = void $ receiveOutstandingResponseMsgsSafely thisThreadId conn qryId

newtype PostgresError = PostgresError {pgErrorDetails :: Map ErrorDetail LBS.ByteString}
  deriving stock (Show)

instance Exception PostgresError

-- | If you receive this exception, don't run any further SQL statements or use it for anything. Just close the connection with `closeForcefully` and discard it.
data IrrecoverableHpgsqlError = IrrecoverableHpgsqlError {hpgsqlDetails :: String, pgErrorDetails :: Map ErrorDetail LBS.ByteString}
  deriving stock (Show)

instance Exception IrrecoverableHpgsqlError

throwPostgresError :: ErrorResponse -> IO a
throwPostgresError (ErrorResponse errDetailMap) = throw $ PostgresError errDetailMap

throwIrrecoverableError :: (MonadThrow m) => String -> m a
throwIrrecoverableError errMsg = throw $ IrrecoverableHpgsqlError {hpgsqlDetails = errMsg, pgErrorDetails = mempty}

-- | Returns the count of affected rows of the given query.
execute :: HPgConnection -> Query -> IO Int64
execute conn qry = sum <$> executeMany conn [qry]

-- | Apply any number of SQL statements that can be row-returning or count-returning.
execute_ :: HPgConnection -> Query -> IO ()
execute_ conn qry = void $ execute conn qry

consumeResultsIgnoreRows :: HPgConnection -> QueryId -> IO Int64
consumeResultsIgnoreRows conn qryId = do
  (_mRowDesc, resultsStream) <- consumeResults conn qryId
  results <- S.effects resultsStream
  case results of
    Left err -> throwPostgresError err
    Right (CommandComplete n) -> pure n

-- | Returns the count of affected rows only of the supplied
-- queries in order.
executeMany :: HPgConnection -> [Query] -> IO [Int64]
executeMany conn qs = do
  sent <- runPipeline conn $ traverse pipelineCmd qs
  sequenceA sent

-- | Apply any number of SQL statements that can be row-returning or count-returning.
executeMany_ :: HPgConnection -> [Query] -> IO ()
executeMany_ conn qry = void $ executeMany conn qry

mkQuery :: (ToPgRow p) => ByteString -> p -> Query
mkQuery qry p = Query $ NE.singleton $ SingleQuery qry (toPgParams p)

queryWithStreaming :: RowParser a -> HPgConnection -> Query -> IO (Stream (Of a) IO ())
queryWithStreaming rparser conn qry = join $ runPipeline conn $ pipelineS rparser qry

getMyWeakThreadId :: IO WeakThreadId
getMyWeakThreadId = do
  -- We don't keep a reference to `ThreadId` as it can stop threads from getting
  -- runtime exceptions and can prevent dead threads from being garbage-collected.
  -- It's explained somewhere in hackage.
  tid <- myThreadId
  wtid <- mkWeakThreadId tid
  pure $ WeakThreadId wtid (fromThreadId tid)

-- | Sends any number of queries to the backend atomically, or throws an irrecoverable exception
-- if it can't do that. Then runs the continuation.
atomicallyInitiatePipelineOrPanicAndThenConsumeResults :: HPgConnection -> NonEmpty QueryProtocol -> [SomeMessage] -> (NonEmpty QueryId -> IO a) -> IO a
atomicallyInitiatePipelineOrPanicAndThenConsumeResults conn queriesBeingSent allMsgs continuation = do
  thisWeakThreadId <- getMyWeakThreadId
  qryIds <- loopUntilNoPipeline conn (getUniqueQueryStatesForNewPipeline thisWeakThreadId queriesBeingSent) $ \(nextId, lastId) -> do
    -- If this thread is interrupted now, it is ok: only `totalQueriesSent` was bumped, but `currentPipeline`
    -- is still empty (it will be modified once we send all control messages to postgres).
    -- This is Note [Only modify totalQueriesSent]
    let newPipeline = zipWith (\queryIdentifier queryProtocol -> QueryState {queryIdentifier, queryOwner = Just thisWeakThreadId, queryProtocol, responseMsgsState = NoMsgsReceived}) [nextId .. lastId] (NE.toList queriesBeingSent)
    uninterruptibleSendControlMsgs_
      conn
      ( allMsgs,
        do
          st <- STM.readTVar (internalConnectionState conn)
          -- TODO: Rethrow exception if there is an exception in the state
          STM.writeTVar (internalConnectionState conn) $ st {currentPipeline = newPipeline}
      )
    debugPrint $ "+++ Sent QueryIds " ++ show [nextId .. lastId]
    pure $ fmap queryIdentifier (NE.fromList newPipeline)
  continuation qryIds
  where
    getUniqueQueryStatesForNewPipeline :: WeakThreadId -> NonEmpty QueryProtocol -> STM (QueryId, QueryId)
    getUniqueQueryStatesForNewPipeline thisThreadId qryprotos = do
      let sttv = internalConnectionState conn
      st <- STM.readTVar sttv
      when (any ((== Just thisThreadId) . queryOwner) (currentPipeline st)) $ throwIrrecoverableError "Bug in Hpgsql: the pipeline should be empty due to loopUntilNoPipeline"
      case currentPipeline st of
        [] -> do
          -- Reserve N ids
          let nextId = QueryId $ totalQueriesSent st
              lastId = nextId + fromIntegral (length qryprotos) - 1
          -- We only modify `totalQueriesSent` in our internal state in this STM transaction.
          -- Check why in Note [Only modify totalQueriesSent]
          STM.writeTVar sttv (st {totalQueriesSent = totalQueriesSent st + fromIntegral (length qryprotos)})
          pure (nextId, lastId)
        _ -> error "Impossible state. Bug in HPgsql: the pipeline should be empty due to loopUntilNoPipeline"

-- | Checks there is no active pipeline and runs the supplied function with the control-msg lock when there
-- isn't one. If there is an active pipeline, waits until it's done executing (and cancels-drains it if it
-- has been orphaned) until it can run the supplied function.
-- The supplied function runs while the control-msg lock is held, and the supplied STM transaction is used
-- in the acquire-resource phase of 'withControlMsgsLock'.
loopUntilNoPipeline :: forall a b. HPgConnection -> STM a -> (a -> IO b) -> IO b
loopUntilNoPipeline conn lockAcquireStm f = do
  thisWeakThreadId <- getMyWeakThreadId
  drainOrphanedQueriesAndRunWithControlMsgLock conn
  retOrRepeat <- withControlMsgsLock
    conn
    (const $ checkIfCanContinue thisWeakThreadId)
    (const $ pure ())
    $ \case
      Left activePipeline@(QueryState {queryOwner} :| _) -> do
        -- If there is a pipeline, we wait some time before trying again
        -- to avoid a tight loop.
        let intervalMs = killedThreadPollIntervalMs $ connOpts conn
        debugPrint $ "There is a pipeline owned by a different thread (" ++ show queryOwner ++ ") so we (" ++ show thisWeakThreadId ++ ") will try again in " ++ show intervalMs ++ "ms. Pipeline contains: "
        debugPrint $ show activePipeline
        -- TODO: Wait 1s or until internal state changes, whatever comes first.
        threadDelay $ 1000 * intervalMs
        pure Nothing
      Right res -> do
        debugPrint $ "+++ No active pipeline found"
        Just <$> f res
  case retOrRepeat of
    Nothing -> loopUntilNoPipeline conn lockAcquireStm f
    Just ret -> pure ret
  where
    -- \| Returns a `Nothing` if connection state has an active pipeline.
    checkIfCanContinue :: WeakThreadId -> STM (Either (NonEmpty QueryState) a)
    checkIfCanContinue thisThreadId = do
      let sttv = internalConnectionState conn
      st <- STM.readTVar sttv
      when (any ((== Just thisThreadId) . queryOwner) (currentPipeline st)) $ throwIrrecoverableError "A previous query's results from this thread have not been fully consumed, so you must consume results to clear the pipeline"
      case currentPipeline st of
        [] -> Right <$> lockAcquireStm
        (q1 : qs) -> pure (Left $ q1 :| qs) -- This is ESSENTIAL! If multiple threads are blocked on each other, some of them can die and STM won't know when that happens. So we need manual retry mechanisms, which means returning a special value here.

-- | Drains orphaned queries if there are any.
drainOrphanedQueriesAndRunWithControlMsgLock :: HPgConnection -> IO ()
drainOrphanedQueriesAndRunWithControlMsgLock conn = do
  -- Drain results of orphaned queries if necessary
  queriesToDrain <- acquireOwnershipOfOrphanedQueries
  -- Acquire control-msg lock when draining to avoid a race condition where
  -- soon after draining the last query a different thread runs a new query.
  -- We want the supplied `f` function to run on a clean state/pipeline.
  unless (null queriesToDrain) $ do
    debugPrint $ "Going to take control-msg lock to drain " ++ show queriesToDrain
    withControlMsgsLock conn (const $ pure ()) (const $ pure ()) $ const $ do
      -- cancelling statements is idempotent in our tests, so it's ok if
      -- this thread is interrupted and some other thread runs it again
      debugPrint "Cancelling active statement to drain"
      cancelAnyRunningStatement conn
      debugPrint $ "Draining " ++ show queriesToDrain
      let drainUntilError [] = pure ()
          drainUntilError (q : qs) = do
            (_, res) <- consumeResults conn q
            eErrorOrCmdComplete <- S.effects res
            -- If we get an error, we cannot continue to consume the results
            -- of other queries as the whole pipeline is trashed
            case eErrorOrCmdComplete of
              Right _cmdComplete -> drainUntilError qs
              Left _err -> pure ()
      drainUntilError queriesToDrain
  where
    -- \| Returns queries that have been taken possession of by this thread for cancellation and draining
    -- or an empty list if there's no need for that.
    acquireOwnershipOfOrphanedQueries :: IO [QueryId]
    acquireOwnershipOfOrphanedQueries = do
      thisThreadId <- getMyWeakThreadId
      debugPrint $ "+++ I am " ++ show thisThreadId ++ " and will look for orphaned queries to drain"
      withControlMsgsLock
        conn
        (\sttv -> currentPipeline <$> STM.readTVar sttv)
        (const $ pure ())
        $ \activeQueries -> do
          -- TODO: We should either move the WeakThreadId owner into the full pipeline,
          -- or change this to a `takeWhile` because the internal model allows different
          -- queries to have different owners, even if in practice that shouldn't happen.
          mustTakeOwnership <- fmap (foldl' (||) False) $ forM activeQueries $ \QueryState {queryOwner} -> case queryOwner of
            Nothing -> pure True
            -- See Note [`timeout` uses the same ThreadId] for why having the same ThreadId _still_ means
            -- we need to cancel and drain those queries
            Just tid -> if tid == thisThreadId then pure True else threadDoesNotExist tid
          if mustTakeOwnership
            then do
              STM.atomically $ do
                st <- STM.readTVar (internalConnectionState conn)
                STM.writeTVar (internalConnectionState conn) $ st {currentPipeline = map (\qs -> qs {queryOwner = Just thisThreadId}) $ currentPipeline st}
              let owner = map queryOwner activeQueries
              debugPrint $ "We (" ++ show thisThreadId ++ ") took ownership of the pipeline orphaned by " ++ show owner
              pure $ map queryIdentifier activeQueries
            else pure []
    threadDoesNotExist :: WeakThreadId -> IO Bool
    threadDoesNotExist (WeakThreadId wtid _) =
      deRefWeak wtid >>= \case
        Nothing -> pure True
        Just tid -> (`elem` [ThreadDied, ThreadFinished]) <$> threadStatus tid

data Pipeline a = Pipeline [(SingleQuery, Maybe [Format])] (HPgConnection -> [QueryId] -> a)
  deriving stock (Functor)

instance Applicative Pipeline where
  -- TODO: Test that this Applicative instance obeys Applicative laws
  pure x = Pipeline [] (\_ _ -> x)
  Pipeline queries runFunc <*> Pipeline moreQueries run2 = Pipeline (queries ++ moreQueries) $ \conn qryIds ->
    let (firstQueries, lastQueries) = List.splitAt (length queries) qryIds
        f = runFunc conn firstQueries
        g = run2 conn lastQueries
     in f g

pipelineS :: RowParser a -> Query -> Pipeline (IO (Stream (Of a) IO ()))
pipelineS rowparser@(RowParser _ _ expectedColFmts) (Query (lastAndInitNE -> (firstQueriesToSend, lastQueryToSend))) =
  Pipeline
    (map (,Nothing) firstQueriesToSend ++ [(lastQueryToSend, Just expectedColFmts)])
    ( \conn qryIds -> do
        case lastAndInit qryIds of
          (firstQueries, mLastQry) -> do
            forM_ firstQueries $ consumeResultsIgnoreRows conn
            pure $ consumeStreamingResults rowparser conn (fromMaybe (error "pipelineS internal bug: no mLastQry") mLastQry)
    )

pipelineL :: RowParser a -> Query -> Pipeline (IO [a])
pipelineL rowparser q = join . fmap S.toList_ <$> pipelineS rowparser q

pipelineCmd :: Query -> Pipeline (IO Int64)
pipelineCmd (Query qs) =
  Pipeline
    (map (,Nothing) (NE.toList qs))
    ( \conn qryIds -> do
        case lastAndInit qryIds of
          (firstQueries, mLastQry) -> do
            forM_ firstQueries $ consumeResultsIgnoreRows conn
            consumeResultsIgnoreRows conn (fromMaybe (error "pipelineCmd internal bug: no mLastQry") mLastQry)
    )

-- | IMPORTANT: Do not consume query results from the same pipeline in different threads. The thread
-- that sends the pipeline must consume the results of every query in the pipeline itself, and in order.
-- Anything else is not officially supported by HPgsql and may result in deadlocks or worse: undefined behaviour.
runPipeline :: HPgConnection -> Pipeline a -> IO a
runPipeline conn (Pipeline [] run) = pure $ run conn []
runPipeline conn (Pipeline (NE.fromList -> queries) run) = do
  let toMessages (SingleQuery qryString qryParams, mRowInfo) =
        [ SomeMessage $ Parse qryString (map fst qryParams),
          SomeMessage $
            Bind
              { paramsValuesInOrder = map snd qryParams,
                resultColumnFmts = case mRowInfo of
                  Nothing -> [BinaryFmt]
                  Just expectedColFmts -> expectedColFmts
              },
          SomeMessage Describe,
          SomeMessage Execute
        ]
  atomicallyInitiatePipelineOrPanicAndThenConsumeResults
    conn
    (fmap (const ExtendedQuery) queries)
    (concatMap toMessages queries ++ [SomeMessage Sync])
    $ \qryIds -> pure $ run conn (NE.toList qryIds)

consumeStreamingResults :: RowParser a -> HPgConnection -> QueryId -> (Stream (Of a) IO ())
consumeStreamingResults (RowParser rparser rtypecheck expectedColFmts) conn qryId = S.effect $ do
  (mERowDesc, rowsStream) <- consumeResults conn qryId
  case mERowDesc of
    Nothing -> do
      -- This is likely an error that happened when binding parameters (e.g. more/fewer params necessary than were sent)
      -- or a query that has no parameters and fails very early (e.g. "SELECT 1/0")
      rowCount :> res <- S.length rowsStream
      when (rowCount > 0) $ throwIrrecoverableError "Bug in HPgsql. We didn't get either NoData or RowDescription, so we assumed there was an error binding the query, but we got more than 0 rows in results"
      case res of
        Left err -> throwPostgresError err
        Right _cmd -> throwIrrecoverableError "Bug in HPgsql. We didn't get either NoData or RowDescription, so we assumed there was an error binding the query, but we then received a CommandComplete."
    Just (Left3 _noData) -> throwIrrecoverableError "You have sent a count-returning query but expected it to be a rows-returning query, so we are aborting."
    Just (Right3 _copyInResponse) -> throwIrrecoverableError "You have sent a COPY FROM STDIN query but expected it to be a rows-returning query, so we are aborting."
    Just (Middle3 (RowDescription coltypes)) -> do
      let numResultColumns = length coltypes
          expectedNumCols = length expectedColFmts
      unless (numResultColumns == expectedNumCols) $ throwIrrecoverableError $ "Query result contains " ++ show numResultColumns ++ " columns but row parser expected " ++ show expectedNumCols
      unless (rtypecheck coltypes) $ throwIrrecoverableError "Query result column types do not match expected column types"
      let rowparser = rparser coltypes <* Parsec.endOfInput
      pure $ do
        errOrCmdComplete <-
          S.mapM
            ( \(DataRow rowColumnData) ->
                case LazyParsec.parseOnly rowparser rowColumnData of
                  Right row -> pure row
                  Left err -> throwIrrecoverableError $ "Failed parsing a row: " ++ show err
            )
            rowsStream
        S.effect $ case errOrCmdComplete of
          Left err -> throwPostgresError err
          Right _cmdComplete -> pure mempty

query :: forall a. (FromPgRow a) => HPgConnection -> Query -> IO [a]
query = queryWith (rowParser @a)

queryWith :: RowParser a -> HPgConnection -> Query -> IO [a]
queryWith rparser conn qry = join $ runPipeline conn $ pipelineL rparser qry

withCopy :: HPgConnection -> Query -> IO () -> IO Int64
withCopy conn (Query (lastAndInitNE -> (firstQueries, SingleQuery {..}))) copyFn = do
  whenNonEmpty firstQueries $ \fqne -> runPipeline conn $ pipelineCmd (Query fqne)
  thisThreadId <- getMyWeakThreadId
  atomicallyInitiatePipelineOrPanicAndThenConsumeResults
    conn
    (CopyQuery StillCopying :| [])
    [ SomeMessage $ Parse queryString (map fst queryParams),
      SomeMessage $ Bind {paramsValuesInOrder = map snd queryParams, resultColumnFmts = []},
      -- We don't send Msgs.Describe because we expect CopyInResponse in place of NoData
      SomeMessage Execute,
      SomeMessage Msgs.Flush -- This might not be necessary for COPY, but possibly useful if the user calls this with not-a-COPY statement so we get errors earlier?
    ]
    $ \(qryId :| _) -> do
      void $ receiveOutstandingResponseMsgsSafely thisThreadId conn qryId
      void $ receiveOutstandingResponseMsgsSafely thisThreadId conn qryId
      void $ receiveOutstandingResponseMsgsSafely thisThreadId conn qryId
      copyFn
      copyEnd conn qryId

copyEnd :: HPgConnection -> QueryId -> IO Int64
copyEnd conn qryId = do
  uninterruptibleSendControlMsgs_
    conn
    ( [SomeMessage CopyDone, SomeMessage Sync],
      do
        -- When CopyDone+Sync are sent, we must update the connection state
        let sttv = internalConnectionState conn
        st <- STM.readTVar sttv
        case currentPipeline st of
          [q@QueryState {queryProtocol = CopyQuery StillCopying}] -> STM.writeTVar sttv $ st {currentPipeline = [q {queryProtocol = CopyQuery CopyDoneAndSyncSent}]}
          _ -> throwIrrecoverableError "putCopyEnd called but there was no active COPY statement"
    )
  (_, stream) <- consumeResults conn qryId
  res <- S.effects stream
  case res of
    Left err -> throwPostgresError err
    Right (CommandComplete n) -> pure n

-- | A simpler version of `uninterruptibleSendControlMsgs`.
uninterruptibleSendControlMsgs_ :: HPgConnection -> ([SomeMessage], STM ()) -> IO ()
uninterruptibleSendControlMsgs_ conn (msgs, stateUpdate) = uninterruptibleSendControlMsgs conn (const $ pure ()) (const $ pure ()) (const $ pure ((), msgs, stateUpdate))

data SomeMessage = forall msg. (ToPgMessage msg) => SomeMessage msg

instance ToPgMessage SomeMessage where
  toPgMessage (SomeMessage msg) = toPgMessage msg

-- | Sends messages by running the supplied function in a way that is
-- semantically equivalent to being atomic, and guarantees the success
-- STM transaction will be applied when the messages were sent, even
-- if this thread is killed.
-- This is an essential message-sending primitive that guarantees that
-- if you send a series of messages, that the internal connection
-- state will be updated once they're sent, and other callers calling this
-- will wait for previous messages to be sent, so they see the same state
-- they would under normal/error-free operation.
uninterruptibleSendControlMsgs :: HPgConnection -> (TVar InternalConnectionState -> STM a) -> (TVar InternalConnectionState -> STM ()) -> (a -> IO (b, [SomeMessage], STM ())) -> IO b
uninterruptibleSendControlMsgs conn acquire release f =
  withControlMsgsLock
    conn
    acquire
    release
    $ \acqValue ->
      modifyMVar (sendBuffer conn) $
        \msgsInBuffer -> do
          (ret, msgs, afterSentTxn) <- f acqValue
          -- Use a DList for appends? Probably not worth it since there are so few control messages.
          pure (msgsInBuffer ++ [(mconcat (map (Builder.toLazyByteString . toPgMessage) msgs), afterSentTxn)], ret)

putCopyData :: HPgConnection -> ByteString -> IO ()
putCopyData conn t = interruptibleSendMsg conn (CopyData t)

-- | This function is thread-and-interruption-safe, so you
-- can run it with the same connection in parallel to any other functions.
getNotificationNonBlocking :: HPgConnection -> IO (Maybe NotificationResponse)
getNotificationNonBlocking conn = STM.atomically $ updateConnStateTxn conn $ \sttv -> do
  st <- STM.readTVar sttv
  STM.tryReadTQueue (notificationsReceived st)

-- | Blocks until a new Notification arrives. This function is both thread-safe and
-- interruption-safe, so you can run it with the same connection in parallel to any
-- other functions.
getNotification :: HPgConnection -> IO NotificationResponse
getNotification conn =
  -- This implementation blocks concurrency in some cases, but should be optimised
  -- for the most common case of no concurrency, and should be correct.
  getNonBlockingOr $
    loopUntilNoPipeline conn (pure ()) $ \() ->
      -- Another query might have filled our internal notification queue while we were
      -- draining, so check that first.
      getNonBlockingOr $
        receiveNextMsgWithMaskedContinuation conn (msgParser @NotificationResponse) pure
  where
    getNonBlockingOr f = do
      mNotifFromQueue <- getNotificationNonBlocking conn
      case mNotifFromQueue of
        Just notif -> pure notif
        Nothing -> f

interruptibleSendMsg :: (ToPgMessage msg, Show msg) => HPgConnection -> msg -> IO ()
interruptibleSendMsg HPgConnection {socket} msg = do
  SocketLBS.sendAll socket $ Builder.toLazyByteString $ toPgMessage msg
  debugPrint $ "Sent " ++ show msg

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Nothing -> pure ()
  Just v -> f v

{-# NOINLINE _globalLock #-}
_globalLock :: MVar ()
_globalLock = unsafePerformIO $ newMVar ()

debugPrint :: String -> IO ()
debugPrint _ = pure ()

-- debugPrint str = modifyMVar_ _globalLock $ const $ putStrLn str

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs

lastAndInit :: [a] -> ([a], Maybe a)
lastAndInit xs = case NE.nonEmpty xs of
  Nothing -> ([], Nothing)
  Just nxs -> second Just $ lastAndInitNE nxs

lastAndInitNE :: NonEmpty a -> ([a], a)
lastAndInitNE (x :| []) = ([], x)
lastAndInitNE (x :| xs) =
  let (others, l) = lastAndInitNE (NE.fromList xs)
   in (x : others, l)

whenNonEmpty :: [a] -> (NonEmpty a -> IO b) -> IO ()
whenNonEmpty [] _ = pure ()
whenNonEmpty (x : xs) f = void $ f (x :| xs)

-- whenM :: IO Bool -> IO a -> IO ()
-- whenM cond f = do
--   v <- cond
--   when v $ void f

ifM :: IO Bool -> IO a -> IO a -> IO a
ifM cond fTrue fFalse = do
  v <- cond
  if v then fTrue else fFalse

-- Note [Polling Weak ThreadId instead of finalizers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- When a connection is shared across multiple threads, and one such thread
-- is interrupted by an asynchronous exception after sending a query to
-- postgres but before consuming its results, the other thread will still
-- be blocked in sending its query until it can know the first thread has died.
--
-- One way to implement this is by adding finalizers on `ThreadId` that would
-- then STM.writeTVar and set `queryOwner = Nothing`, so good old `STM.retry`
-- would make blocked threads retry promptly when that happens.
--
-- However, it is possible to `addFinalizer`, but not to `removeFinalizer`.
-- And that means every new sent query adds a new finalizer, and these accumulate
-- (leak).
-- There are strategies to only register a finalizer once, but that requires
-- keeping state that grows with the number of threads a connection is picked up by.
--
-- I did implement the strategy with finalizers at some point, but thought
-- it wasn't worth the risk, and then switched to polling `deRefWeak` instead.
--
-- One other interesting thing is that I've seen GHC's runtime take a while to
-- collect ThreadIds, so forcing a garbage collection in between waits helps
-- it realise more promptly.
-- This isn't done for users of the library; we do it only in our tests, because
-- we hope real world applications allocate memory a lot more than our tests, so
-- they trigger collections more often. Or one can hope.
