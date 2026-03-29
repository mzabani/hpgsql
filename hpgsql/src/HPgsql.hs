module HPgsql
  ( query,
    queryWith,
    queryWithStreaming,
    queryStreaming,
    copyStart,
    copyEnd,
    putCopyData,
    withCopy,
    withConnection,
    withConnectionOpts,
    ConnString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    RowParser (..),
    HPgConnection, -- Don't expose constructor
    Query, -- Don't expose constructor
    TransactionStatus (..),
    Only (..),
    FromPgRow (..),
    FromPgField (..),
    NotificationResponse (..),
    Pipeline, -- Don't expose constructor
    PoolCleanup (..),
    beforeReturningToPool,
    cancelAnyRunningStatement,
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
    runPipeline,
    pipelineCmd,
    pipelineL,
    pipelineS,
    getBackendPid,
    getNotification,
    getNotificationNonBlocking,
    refreshTypeInfoCache,
    resetTypeInfoCache,
  )
where

import Control.Applicative (Alternative (..))
import Control.Concurrent (mkWeakThreadId, modifyMVar, modifyMVar_, myThreadId, readMVar)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (MonadThrow, bracket, bracketOnError, finally, handle, mask, mask_, onException, throw, tryJust)
import Control.Monad (forM, forM_, join, unless, void, when)
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.Attoparsec.ByteString.Lazy as LazyParsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight)
import Data.Int (Int32, Int64)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Serialize as Cereal
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import Data.Time (DiffTime, diffTimeToPicoseconds, secondsToDiffTime)
#if MIN_VERSION_base(4,19,0)
import GHC.Conc.Sync (ThreadStatus (..), fromThreadId, threadStatus)
#else
import GHC.Conc.Sync (ThreadStatus (..), showThreadId, threadStatus)
#endif
import HPgsql.Base
import HPgsql.Connection (ConnString (..))
import HPgsql.Encoding (ColumnInfo (..), FromPgField (..), FromPgRow (..), Only (..), RowParser (..))
import HPgsql.InternalTypes (BindComplete (..), CommandComplete (..), ConnectOpts (..), CopyInResponse (..), CopyQueryState (..), DataRow (..), Either3 (..), EncodingContext (..), ErrorDetail (..), ErrorResponse (..), HPgConnection (..), InternalConnectionState (..), IrrecoverableHpgsqlError (..), NoData (..), NotificationResponse (..), ParseComplete (..), Pipeline (..), PoolCleanup (..), PostgresError (..), QueryId (..), QueryProtocol (..), QueryState (..), ReadyForQuery (..), ResponseMsg (..), ResponseMsgsReceived (..), RowDescription (..), TransactionStatus (..), WeakThreadId (..))
import HPgsql.Msgs (AuthenticationOk, BackendKeyData (..), Bind (..), CancelRequest (..), CopyData (..), CopyDone (..), Describe (..), Execute (..), FromPgMessage (..), NoticeResponse (..), ParameterStatus (..), Parse (..), PgMsgParser (..), StartupMessage (..), Sync (..), Terminate (..), ToPgMessage (..), parsePgMessage)
import qualified HPgsql.Msgs as Msgs
import HPgsql.Networking (recvNonBlocking, sendNonBlocking, socketWaitRead, socketWaitWrite)
import HPgsql.Query (Query (..), SingleQuery (..), breakQueryIntoStatements)
import HPgsql.TypeInfo (Format (..), TypeInfo (..), builtinPgTypesMap)
import Network.Socket (AddrInfo (..))
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as SocketLBS
import Streaming (Of (..), Stream)
import qualified Streaming as S
import qualified Streaming.Internal as SInternal
import qualified Streaming.Prelude as S
import System.IO.Error (isResourceVanishedError)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (deRefWeak)
import System.Timeout (timeout)

-- | Returns a Left with the current pipeline if connection is not ready for a new pipeline, a Right
-- with the current transaction status otherwise.
connectionReadyForNewPipeline :: InternalConnectionState -> Either (NonEmpty QueryState) TransactionStatus
connectionReadyForNewPipeline (currentPipeline -> pipeline) =
  -- You can tell there are two ways to represent no active pipeline, aka being ready
  -- to send a new query: and empty pipeline and a pipeline with a ReadyForQuery received.
  -- This might seem silly, but it really helps resuming interrupted execution from a point
  -- when ReadyForQuery was already received, because we can still associate a ReadyForQuery
  -- with a QueryId.
  -- The empty list should only exist immediately after connecting and before the very first
  -- query is sent. After that it's never empty again as new pipelines replace old ones.
  case pipeline of
    [] -> Right TransIdle
    (q1 : qs) -> case headMaybe $
      mapMaybe
        ( \qstate -> case responseMsgsState qstate of
            ReadyForQueryReceived _ (ReadyForQuery s) -> Just s
            _ -> Nothing
        )
        pipeline of
      Nothing -> Left (q1 :| qs)
      Just st -> Right st

connectionTransactionStatus :: HPgConnection -> IO TransactionStatus
connectionTransactionStatus conn = STM.atomically $ updateConnStateTxn conn $ \sttv -> do
  st <- STM.readTVar sttv
  case connectionReadyForNewPipeline st of
    Right s -> pure s
    Left pipeline ->
      pure $
        if any
          ( ( \case
                ErrorResponseReceived _ _ -> True
                _ -> False
            )
              . responseMsgsState
          )
          pipeline
          then
            TransInError
          else TransInTrans

connect :: ConnString -> DiffTime -> IO HPgConnection
connect =
  connectOpts defaultConnectOpts

connectOpts :: ConnectOpts -> ConnString -> DiffTime -> IO HPgConnection
connectOpts =
  internalConnectOrCancel
    Connect

defaultConnectOpts :: ConnectOpts
defaultConnectOpts =
  ConnectOpts
    { killedThreadPollIntervalMs = 500,
      cancellationRequestResendIntervalMs = 500,
      fillTypeInfoCache = True
    }

data InternalConnectOrCancelRequest a where
  Connect :: InternalConnectOrCancelRequest HPgConnection
  CancelNotConnect :: CancelRequest -> AddrInfo -> InternalConnectOrCancelRequest ()

internalConnectOrCancel :: InternalConnectOrCancelRequest a -> ConnectOpts -> ConnString -> DiffTime -> IO a
internalConnectOrCancel connectOrCancel connOpts originalConnStr@ConnString {..} conntimeout = do
  -- TODO: Proper exception rethrowing when we fail to connect
  sockOrTimeout <- timeout (fromInteger $ diffTimeToPicoseconds conntimeout `div` 1_000_000) $ getConnectedSocket Nothing
  case sockOrTimeout of
    Nothing -> throwIrrecoverableError "Could not connect in the supplied timeout"
    -- TODO: It's still possible for an asynchronous exception to interrupt this before the `onException` handler is installed
    Just (sock, addrInfo) -> flip onException (Socket.close sock) $ do
      Socket.withFdSocket sock Socket.getNonBlock >>= \case
        False -> throwIrrecoverableError "Socket is not marked as non-blocking, which is not supported by hpgsql. You might be running on an unsupported platform"
        True -> pure ()
      recvBuffer <- newMVar mempty
      sendBuffer <- newMVar mempty
      encodingContext <- newMVar (EncodingContext builtinPgTypesMap)
      connParams <- newMVar mempty
      notifQueue <- STM.newTQueueIO
      currentConnectionState <-
        STM.newTVarIO $
          InternalConnectionState
            { totalQueriesSent = 0,
              blockedForSendingOrReceivingMsgsAtomically = Nothing,
              currentPipeline = [],
              notificationsReceived = notifQueue
            }
      let hpgConnPartialDoNotReturn = HPgConnection sock recvBuffer sendBuffer originalConnStr addrInfo encodingContext connParams currentConnectionState 0 0 connOpts
      case connectOrCancel of
        CancelNotConnect cancelRequest _ -> do
          -- TODO: We need to store the IP address of the server and reuse that,
          -- because name resolution might give us a different IP address and thus
          -- a different server!
          nonAtomicSendMsg hpgConnPartialDoNotReturn cancelRequest
          -- We _must_ wait until the socket is closed _by the other end_ (PostgreSQL-the-server),
          -- because otherwise this cancellation request might be processed while the client sends
          -- another query. See https://www.postgresql.org/message-id/flat/27126.1126649920%40sss.pgh.pa.us#75364d0966758fccad56cd6c71547771
          void $ tryJust (\err -> if isResourceVanishedError err then Just () else Nothing) $ Socket.waitReadSocketSTM (socket hpgConnPartialDoNotReturn) >>= STM.atomically
          Socket.close sock
        Connect -> do
          -- TODO: Send encoding and other things with "options"?
          nonAtomicSendMsg hpgConnPartialDoNotReturn $ StartupMessage {user, database, options}
          void $ receiveNextMsgUnsafe hpgConnPartialDoNotReturn (msgParser @AuthenticationOk)
          errorOrBackendKeyData <- receiveNextMsgUnsafe hpgConnPartialDoNotReturn $ Right <$> msgParser @BackendKeyData <|> Left <$> msgParser @ErrorResponse
          -- TODO: Throw informative error for unimplemented authentication methods
          case errorOrBackendKeyData of
            Left (ErrorResponse errDetails) -> throw $ IrrecoverableHpgsqlError {hpgsqlDetails = "Socket connected but postgresql threw an error during connection startup handshake", pgErrorDetails = errDetails, innerException = Nothing, relatedStatement = Nothing}
            Right backendKeyData -> do
              readyForQueryOrError <- receiveNextMsgUnsafe hpgConnPartialDoNotReturn $ Right <$> msgParser @ReadyForQuery <|> Left <$> msgParser @ErrorResponse
              case readyForQueryOrError of
                Left (ErrorResponse errDetails) -> throw $ IrrecoverableHpgsqlError {hpgsqlDetails = "Some postgresql error happened while connecting", pgErrorDetails = errDetails, innerException = Nothing, relatedStatement = Nothing}
                Right ReadyForQuery {} -> pure ()
              debugPrint $ "Connected with backend PID " ++ show (backendPid backendKeyData)
              let finalConn = hpgConnPartialDoNotReturn {connPid = backendPid backendKeyData, cancelSecretKey = backendSecretKey backendKeyData}
              when (fillTypeInfoCache connOpts) $ join $ runPipeline finalConn $ refreshTypeInfoCache finalConn
              pure finalConn
  where
    -- \| Unsafe version of `withSafeReceiveNextMsg`.
    receiveNextMsgUnsafe :: (Show a) => HPgConnection -> PgMsgParser a -> IO a
    receiveNextMsgUnsafe conn parser = receiveNextMsgWithMaskedContinuation conn parser pure

    -- TODO: Get ParameterStatus and set client_encoding to UTF8 if it isn't
    getConnectedSocket resolvedAddr = do
      addrInfo <- case resolvedAddr of
        Just addrInfo -> pure addrInfo
        Nothing ->
          case connectOrCancel of
            CancelNotConnect _ addrInfo -> pure addrInfo
            Connect ->
              if "/" `List.isInfixOf` hostname
                then
                  pure
                    AddrInfo
                      { addrFlags = [],
                        addrFamily = Socket.AF_UNIX,
                        addrSocketType = Socket.Stream,
                        addrProtocol = Socket.defaultProtocol,
                        addrAddress = Socket.SockAddrUnix $ List.dropWhileEnd (== '/') hostname ++ "/.s.PGSQL." ++ show port,
                        addrCanonName = Nothing
                      }
                else do
                  addrInfos <- Socket.getAddrInfo (Just Socket.defaultHints) (Just hostname) (Just $ show port)
                  case addrInfos of
                    [] -> throwIrrecoverableError "Could not resolve address"
                    addrInfo : _ -> pure addrInfo
      sock <- Socket.openSocket addrInfo
      Socket.connect sock (Socket.addrAddress addrInfo)
      pure (sock, addrInfo)

-- | Fetches custom types from postgres and refreshes this connection's
-- internal typeInfo cache with them.
-- Note that builtin postgres types are always available in the typeInfo cache;
-- this just refreshes any other custom types, including removing those that
-- no longer exist and adding new ones.
-- HPgsql runs this automatically for new connections unless you disable it.
-- This is a Pipeline so you can batch it with other commands for reduced latency.
-- See `ConnectOpts` and `resetTypeInfoCache` for more.
refreshTypeInfoCache :: HPgConnection -> Pipeline (IO ())
refreshTypeInfoCache conn =
  -- TODO: Expose as a Pipeline so users can batch it with other typical
  -- connection-setup statements?
  -- https://www.postgresql.org/docs/current/system-catalog-initial-data.html#SYSTEM-CATALOG-OID-ASSIGNMENT
  -- says "OIDs assigned during normal database operation are constrained to be 16384 or higher. This ensures that the range 10000—16383 is free for OIDs assigned automatically by genbki.pl or during initdb. These automatically-assigned OIDs are not considered stable, and may change from one installation to another."
  let fetchPipeline = pipelineL rowParser "select oid, typname, typarray from pg_catalog.pg_type WHERE oid >= 16384"
   in fillTypeInfoCache <$> fetchPipeline
  where
    fillTypeInfoCache queryResultsIO = do
      queryResults <- queryResultsIO
      let customTypes = Map.fromList $ map (\(oid, typname, typarray) -> (oid, TypeInfo typname (if typarray == 0 then Nothing else Just typarray))) queryResults
      modifyMVar_ conn.encodingContext $ \_ -> pure $ EncodingContext $ customTypes `Map.union` builtinPgTypesMap

-- | Useful to reset the connection's internal typeInfo cache
-- to the builtin postgres types.
resetTypeInfoCache :: HPgConnection -> IO ()
resetTypeInfoCache conn = modifyMVar_ conn.encodingContext $ \_ -> pure $ EncodingContext builtinPgTypesMap

getParameterStatus :: HPgConnection -> Text -> IO (Maybe Text)
getParameterStatus HPgConnection {parameterStatusMap} paramName = Map.lookup paramName <$> readMVar parameterStatusMap

getBackendPid :: HPgConnection -> Int32
getBackendPid HPgConnection {connPid} = connPid

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
    when (isLeft $ connectionReadyForNewPipeline st) $ throwIrrecoverableError "There are still active queries in progress. Make sure to close this connection with `closeForcefully` or consume all existing queries' results"
  when (checkTransactionState cleanOpts) $ do
    txnStatus <- connectionTransactionStatus conn
    unless (txnStatus == TransIdle) $ throwIrrecoverableError $ "The connection's transaction was left in an invalid state: " ++ show txnStatus ++ ". Make sure to close this connection with `closeForcefully`"
  -- What if there are notifications in the socket buffer? It seems reasonable to assume that when
  -- running "UNLISTEN *" those would be received, so this might be fine as long as we
  -- clear the internal queue _after_ "UNLISTEN *".
  executeMany_ conn $ (if resetAll cleanOpts then ["RESET ALL", "RESET ROLE"] else []) ++ (if unlistenAll cleanOpts then ["UNLISTEN *"] else [])
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
    (const $ pure ())
    $ \() -> do
      nonAtomicSendMsg conn Terminate

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
receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure conn@HPgConnection {socket, recvBuffer} parser f = rethrowAsIrrecoverable $ do
  -- \^ We rethrow as irrecoverable because an error here is likely a socket receiving error.
  --
  -- We need to preserve the invariant that the internal buffer's first byte is
  -- always the first byte of a valid Message while keeping this function
  -- interruptible.
  -- This means we can't extract a message partially from the internal buffer,
  -- even in face of asynchronous exceptions.
  -- So we append to the buffer up until it has been fully fetched,
  -- and then extract it from the buffer in one piece.
  currentBuf <- receiveUntilBufferHasAtLeast 5
  let bufLen = LBS.length currentBuf
  let charAndLength = LBS.take 5 currentBuf
  let (w2c -> msgIdentChar, lenbs) = fromMaybe (error "impossible") $ LBS.uncons charAndLength
      lenLeftToFetch :: Int64 = fromIntegral $ either error id (Cereal.decodeLazy @Int32 lenbs) - 4
      fullMessageLen = 5 + lenLeftToFetch
  restOfMsg <- LBS.drop 5 . LBS.take fullMessageLen <$> if bufLen >= fullMessageLen then pure currentBuf else receiveUntilBufferHasAtLeast fullMessageLen
  receivedNoticeOrParameterSoTryAgain <- go msgIdentChar restOfMsg fullMessageLen
  -- We don't let `go` recursively call itself or even `receivedNoticeOrParameterSoTryAgain`
  -- because it would inherit its own masking, which would be bad!
  -- We don't want to have to test the behaviour of our functions with both
  -- masking and unmasking!
  case receivedNoticeOrParameterSoTryAgain of
    Nothing -> receiveNextMsgWithMaskedContinuationButDontThrowOnParsingFailure conn parser f
    Just res -> pure res
  where
    go msgIdentChar restOfMsg fullMessageLen = mask_ $ do
      case parsePgMessage msgIdentChar restOfMsg parser of
        Just msg -> do
          removeFirstBytesFromBufferOrThrow fullMessageLen
          debugPrint $ "Received " ++ show msg
          -- sttv <- STM.atomically $ STM.readTVar (internalConnectionState conn)
          -- debugPrint $ "Received " ++ show msg ++ "for pipeline " ++ show (currentPipeline sttv)
          -- putStrLn $ "Received " ++ show msg
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
              modifyMVar_ (parameterStatusMap conn) $ \(!paramMap) -> pure (Map.insert parameterName parameterValue paramMap)
              pure Nothing
            Nothing -> Just <$> f (Left msgIdentChar)

    removeFirstBytesFromBufferOrThrow :: Int64 -> IO ()
    removeFirstBytesFromBufferOrThrow nbytes = modifyMVar_ recvBuffer $ \lbs -> do
      if LBS.length lbs >= nbytes
        then pure (LBS.drop nbytes lbs)
        else
          error "Bug in HPgsql. Internal buffer's bytes weren't filled enough"

    -- \| Appends into the internal buffer by reading from the socket
    -- until the buffer has at least N bytes.
    -- Returns the current buffer.
    receiveUntilBufferHasAtLeast :: Int64 -> IO LBS.ByteString
    receiveUntilBufferHasAtLeast minBytesNecessary = do
      currentBuffer <- readMVar recvBuffer
      let nBytesInBuffer = LBS.length currentBuffer
      if nBytesInBuffer >= minBytesNecessary
        then pure currentBuffer
        else do
          -- This takes from the kernel's recv buffer and appends to our buffer atomically,
          -- or an exception is thrown when receiving.
          modifyMVar_ recvBuffer $ \lbs -> mask $ \restore -> do
            restore $ socketWaitRead socket
            someBytes <- timeDebugNonBlockingOperation "recv" $ recvNonBlocking socket (max 16000 $ fromIntegral $ minBytesNecessary - nBytesInBuffer)
            pure (lbs <> LBS.fromStrict someBytes)
          receiveUntilBufferHasAtLeast minBytesNecessary

sendCancellationRequest :: HPgConnection -> IO ()
sendCancellationRequest conn = do
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
      atomicallySendControlMsgs_ conn ([SomeMessage $ Msgs.CopyFail "COPY statement automatically cancelled by HPgsql because it was interrupted", SomeMessage Sync], markCopyFailSent)
    Just CopyDoneAndSyncSent ->
      pure
        () -- Already finished, nothing to cancel
    Just CopyFailAndSyncSent ->
      pure
        () -- Already cancelled, no need to send another
    Nothing ->
      internalConnectOrCancel
        (CancelNotConnect (CancelRequest (connPid conn) (cancelSecretKey conn)) (connectedTo conn))
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
updateConnStateTxn conn f = f (internalConnectionState conn)

withControlMsgsLock ::
  HPgConnection ->
  -- | Applied in the same STM transaction that takes the lock
  (TVar InternalConnectionState -> STM a) ->
  -- | Applied in the same STM transaction that releases the lock
  (TVar InternalConnectionState -> STM b) ->
  (a -> IO c) ->
  IO c
withControlMsgsLock conn@HPgConnection {socket} acqStm relStm f = do
  thisThreadId <- getMyWeakThreadId
  bracket
    ( STM.atomically $ do
        let sttv = internalConnectionState conn
        st <- STM.readTVar sttv
        let blockedBy = blockedForSendingOrReceivingMsgsAtomically st
        newSt <- case blockedBy of
          Nothing -> pure $ st {blockedForSendingOrReceivingMsgsAtomically {- traceShowWith ("Grabbing ",) $ -} = Just (thisThreadId, 1)}
          Just (tid, nGrabs) ->
            if tid == thisThreadId then pure $ st {blockedForSendingOrReceivingMsgsAtomically {- traceShowWith ("Grabbing ",) $ -} = Just (thisThreadId, nGrabs + 1)} else STM.retry
        STM.writeTVar sttv newSt
    )
    -- Release lock on success or error
    ( const $ void $ STM.atomically $ do
        let sttv = internalConnectionState conn
        st <- STM.readTVar sttv
        let blockedBy = blockedForSendingOrReceivingMsgsAtomically st
        newSt <- case blockedBy of
          Nothing -> throwIrrecoverableError "Impossible: should have been blocked but was not!"
          Just (tid, nGrabs) ->
            let newLockState = {- traceShowWith ("Releasing ",) $ -} if nGrabs <= 1 then Nothing else Just (thisThreadId, nGrabs - 1)
             in if tid == thisThreadId then pure st {blockedForSendingOrReceivingMsgsAtomically = newLockState} else throwIrrecoverableError "Impossible: Lock of a different thread!"
        STM.writeTVar sttv newSt
        -- debugPrint "Internal state: [Released control-msg-lock]."
    )
    $ \() -> do
      -- The STM lock is acquired, now we run flushSendBuffer
      -- so the caller will only see internal state after previous
      -- messages were sent
      flushSendBuffer
      bracket
        (STM.atomically $ updateConnStateTxn conn acqStm)
        (const $ void $ STM.atomically $ updateConnStateTxn conn relStm)
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
    flushSendBuffer =
      -- An exception here could be a socket error or something
      -- that forces us to discard the connection
      rethrowAsIrrecoverable $
        mask $
          \restore -> do
            let go = do
                  others <- modifyMVar (sendBuffer conn) $ \case
                    [] -> pure ([], [])
                    ((!msgs, afterSentTxn) : xs) ->
                      if LBS.null msgs
                        then do
                          -- debugPrint "Finished sending msgs"
                          STM.atomically afterSentTxn
                          pure (xs, xs)
                        else do
                          restore $ socketWaitWrite socket
                          n <- timeDebugNonBlockingOperation "sendNonBlocking" $ sendNonBlocking socket msgs
                          -- debugPrint $ "Sent " ++ show n ++ ". Left: " ++ show (LBS.length (LBS.drop n msgs))
                          let fin = (LBS.drop n msgs, afterSentTxn) : xs
                          pure (fin, fin)

                  -- debugPrint $ show $ stop || null others
                  unless (null others) go
            go

-- | Receives the next response message for the given QueryId atomically, updates
-- internal connection state to reflect it, and returns the updated state alongside the
-- received message.
-- This also receives ReadyForQuery if the query is already in ErrorResponse or if it's the
-- last in the pipeline and has received CommandComplete.
-- If a pipeline has already received ReadyForQuery, this will return that ReadyForQuery
-- without receiving any new messages. This is helpful if a thread is interrupted
-- You don't want to use this for receiving DataRows in large scale, because the costs of
-- STM transactions apply here.
receiveOutstandingResponseMsgsAtomically :: WeakThreadId -> HPgConnection -> QueryId -> IO (Maybe ResponseMsg, ResponseMsgsReceived)
receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId = do
  -- debugPrint $ "Internal state: [Waiting] until ok to receive response control messages for QueryId " ++ show qryId
  withControlMsgsLock
    conn
    -- Check last received message and take lock before receiving next message
    (const getQueryStateIfFirstOrThrow)
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
        [] -> throwIrrecoverableError $ "QueryId " <> show qryId <> " does not exist because the pipeline is empty. This is most likely a bug in HPgsql, but just in case, are you trying to consume a pipeline that no longer exists?"
        queries
          | all ((> qryId) . queryIdentifier) queries -> throwIrrecoverableError $ "Bug in HPgsql: trying to receive outstanding messages for a pipeline that has already been fully consumed. Information about this pipeline no longer available in internal state.: " ++ show (qryId, queries)
          | any ((/= thisThreadId) . queryOwner) queries -> throwIrrecoverableError "HPgsql does not support consuming different SQL statements' results of the same pipeline from different threads. Behaviour is undefined if you try that."
        (splitQueries -> (earlierQueries, thisQuery))
          | any queryInError earlierQueries -> throwIrrecoverableError "Another query in the same pipeline threw an error"
          | not (all queryComplete earlierQueries) -> throwIrrecoverableError "Are you trying to consume a statement's results before consuming the results of previous statements of the same pipeline? HPgsql does not support that. It is also possible a previous statement in the pipeline threw an irrecoverable error, and you still tried to consume another statement's results, which is also not supported."
          | otherwise -> pure thisQuery

    splitQueries :: [QueryState] -> ([QueryState], QueryState)
    splitQueries qs = case List.span ((< qryId) . queryIdentifier) qs of
      (_, []) -> error $ "Could not find query with right Id: " ++ show (qs, qryId)
      (as, firstQuery : _others)
        | queryIdentifier firstQuery == qryId -> (as, firstQuery)
        | otherwise -> error $ "Could not find query right Id (part 2): " ++ show (qs, qryId)

    queryComplete :: QueryState -> Bool
    queryComplete QueryState {responseMsgsState} = case responseMsgsState of
      CommandCompleteReceived _ _ -> True
      _ -> False

    queryInError :: QueryState -> Bool
    queryInError QueryState {responseMsgsState} = case responseMsgsState of
      ErrorResponseReceived _ _ -> True
      ReadyForQueryReceived (Left ErrorResponse {}) _ -> True
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
              -- We could set the pipeline to an empty list when receiving a ReadyForQuery,
              -- and that would be one fewer state to handle, but it disassociates a QueryId
              -- from ReadyForQuery and makes it impossible to resume interruption of `consumeResults`
              -- for a query that has finished executing, leading to bugs if query result
              -- consumption is interrupted in just the right place.
              map
                ( \qs ->
                    if queryIdentifier qs == qryId
                      then
                        qs
                          { responseMsgsState = newState
                          }
                      else qs
                )
                allQueries
          }
      pure (Just respMsg, newState)

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
        nextMsg <- receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
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
                      stateAfterNextMsg <- snd <$> receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
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
    receiveReadyForQueryIfNecessary thisThreadId = void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId

throwPostgresError :: ByteString -> ErrorResponse -> IO a
throwPostgresError stmtText (ErrorResponse errDetailMap) = throw $ PostgresError {pgErrorDetails = errDetailMap, failedStatement = stmtText}

throwIrrecoverableError :: (MonadThrow m) => String -> m a
throwIrrecoverableError errMsg = throw $ IrrecoverableHpgsqlError {hpgsqlDetails = errMsg, pgErrorDetails = mempty, innerException = Nothing, relatedStatement = Nothing}

throwIrrecoverableErrorWithStatement :: (MonadThrow m) => ByteString -> String -> m a
throwIrrecoverableErrorWithStatement stmtText errMsg = throw $ IrrecoverableHpgsqlError {hpgsqlDetails = errMsg, pgErrorDetails = mempty, innerException = Nothing, relatedStatement = Just stmtText}

lookupQueryText :: HPgConnection -> QueryId -> IO ByteString
lookupQueryText conn qryId = STM.atomically $ do
  st <- STM.readTVar (internalConnectionState conn)
  pure $ maybe "" queryText $ List.find ((== qryId) . queryIdentifier) (currentPipeline st)

-- | Returns the count of affected rows of the given query.
execute :: HPgConnection -> Query -> IO Int64
execute conn qry = sum <$> executeMany conn [qry]

-- | Apply any number of SQL statements that can be row-returning or count-returning.
execute_ :: HPgConnection -> Query -> IO ()
execute_ conn qry = void $ execute conn qry

consumeResultsIgnoreRows :: HPgConnection -> QueryId -> IO Int64
consumeResultsIgnoreRows conn qryId = do
  qText <- lookupQueryText conn qryId
  (_mRowDesc, resultsStream) <- consumeResults conn qryId
  results <- S.effects resultsStream
  case results of
    Left err -> throwPostgresError qText err
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

-- | Streams results directly from the connection's socket, i.e. without using cursors.
-- It is important to note the same thread that runs this must be the thread that
-- consumes the returned Stream, and the returned Stream must be consumed completely
-- (up to the last row or a postgres error) before you are able to run other queries.
queryWithStreaming :: RowParser a -> HPgConnection -> Query -> IO (Stream (Of a) IO ())
queryWithStreaming rparser conn qry = join $ runPipeline conn $ pipelineS rparser qry

-- | Streams results directly from the connection's socket, i.e. without using cursors.
-- It is important to note the same thread that runs this must be the thread that
-- consumes the returned Stream, and the returned Stream must be consumed completely
-- (up to the last row or a postgres error) before you are able to run other queries.
queryStreaming :: (FromPgRow a) => HPgConnection -> Query -> IO (Stream (Of a) IO ())
queryStreaming = queryWithStreaming rowParser

getMyWeakThreadId :: IO WeakThreadId
getMyWeakThreadId = do
  -- We don't keep a reference to `ThreadId` as it can stop threads from getting
  -- runtime exceptions and can prevent dead threads from being garbage-collected.
  -- It's explained somewhere in hackage.
  tid <- myThreadId
  wtid <- mkWeakThreadId tid
#if MIN_VERSION_base(4,19,0)
  pure $ WeakThreadId wtid (fromThreadId tid)
#else
  let tidStr = showThreadId tid
  pure $ WeakThreadId wtid tidStr
#endif

-- | Sends any number of queries to the backend atomically, or throws an irrecoverable exception
-- if it can't do that. Then runs the continuation.
atomicallyInitiatePipelineOrPanicAndThenConsumeResults :: HPgConnection -> NonEmpty (ByteString, QueryProtocol) -> [SomeMessage] -> (NonEmpty QueryId -> IO a) -> IO a
atomicallyInitiatePipelineOrPanicAndThenConsumeResults conn queriesBeingSent allMsgs continuation = do
  thisWeakThreadId <- getMyWeakThreadId
  qryIds <- waitUntilPipelineIsReadyForNewQuery conn (getUniqueQueryStatesForNewPipeline queriesBeingSent) $ \(nextId, lastId) -> do
    -- If this thread is interrupted now, it is ok: only `totalQueriesSent` was bumped, but `currentPipeline`
    -- is still empty (it will be modified once we send all control messages to postgres).
    -- This is Note [Only modify totalQueriesSent]
    let newPipelineList = zipWith (\queryIdentifier (queryText, queryProtocol) -> QueryState {queryIdentifier, queryText, queryOwner = thisWeakThreadId, queryProtocol, responseMsgsState = NoMsgsReceived}) [nextId .. lastId] (NE.toList queriesBeingSent)
    case NE.nonEmpty newPipelineList of
      Nothing -> throwIrrecoverableError "Bug in Hpgsql: empty newPipeline to be sent"
      Just newPipeline -> do
        atomicallySendControlMsgs_
          conn
          ( allMsgs,
            do
              st <- STM.readTVar (internalConnectionState conn)
              STM.writeTVar (internalConnectionState conn) $ st {currentPipeline = NE.toList newPipeline}
          )
        debugPrint $ "+++ Sent QueryIds " ++ show [nextId .. lastId]
        pure $ fmap queryIdentifier newPipeline
  continuation qryIds
  where
    getUniqueQueryStatesForNewPipeline :: NonEmpty (ByteString, QueryProtocol) -> STM (QueryId, QueryId)
    getUniqueQueryStatesForNewPipeline (fmap snd -> qryprotos) = do
      let sttv = internalConnectionState conn
      st <- STM.readTVar sttv
      when (isLeft $ connectionReadyForNewPipeline st) $ throwIrrecoverableError "Bug in Hpgsql: the connection should be ready for a new pipeline due to loopUntilNoPipeline"
      -- Reserve N ids
      let nextId = QueryId $ totalQueriesSent st
          lastId = nextId + fromIntegral (length qryprotos) - 1
      -- We only modify `totalQueriesSent` in our internal state in this STM transaction.
      -- Check why in Note [Only modify totalQueriesSent]
      STM.writeTVar sttv (st {totalQueriesSent = totalQueriesSent st + fromIntegral (length qryprotos)})
      pure (nextId, lastId)

-- | Checks there is no active pipeline and runs the supplied function with the control-msg lock when there
-- isn't one. If there is an active pipeline, waits until it's done executing (and cancels-drains it if it
-- has been orphaned) until it can run the supplied function.
-- The supplied STM transaction runs while the control-msg lock is held.
-- DO NOT CALL THIS FUNCTION WHILE HOLDING THE CONTROL-MSGS-LOCK, because it needs to wait/block
-- until the pipeline is ready, which won't happen if we're holding the control-msgs-lock.
waitUntilPipelineIsReadyForNewQuery :: forall a b. HPgConnection -> STM a -> (a -> IO b) -> IO b
waitUntilPipelineIsReadyForNewQuery conn lockAcquireStm f = do
  thisWeakThreadId <- getMyWeakThreadId
  cancelAnyRunningStatement conn
  retOrRepeat <- withControlMsgsLock
    conn
    ( \sttv -> do
        st <- STM.readTVar sttv
        case connectionReadyForNewPipeline st of
          Left p -> pure $ Left p
          Right _ -> Right <$> lockAcquireStm
    )
    (const $ pure ())
    $ \case
      Left (QueryState {queryOwner} :| _) -> pure $ Left queryOwner
      Right acq -> do
        debugPrint "+++ No active pipeline found"
        Right <$> f acq
  case retOrRepeat of
    Left existingPipelineOwnerThread -> do
      -- If there is a pipeline, we must wait while _not_ holding
      -- the control-msgs lock so the other pipeline can reach
      -- completion. Also, we resume immediately if the pipeline
      -- state changes, as it's important to resume quickly to avoid introducing
      -- N * intervalMs delays for a concurrent workload with N
      -- threads blocked waiting on each other.
      let intervalMs = killedThreadPollIntervalMs $ connOpts conn
      debugPrint $ "There is a pipeline owned by a different thread (" ++ show existingPipelineOwnerThread ++ ") so we (" ++ show thisWeakThreadId ++ ") will try again in " ++ show intervalMs ++ "ms. Pipeline contains: "
      void $ timeout (1000 * intervalMs) $ STM.atomically $ do
        st <- STM.readTVar (internalConnectionState conn)
        when (isLeft $ connectionReadyForNewPipeline st) STM.retry
      waitUntilPipelineIsReadyForNewQuery conn lockAcquireStm f
    Right ret -> pure ret

-- | Cancels any running statements in the current connection, including COPY, or returns if there
-- is no active query to cancel.
-- Make sure you do not try to consume results of queries you have already sent if you run this, or
-- behaviour is undefined. That means if you had a Stream result and you run this function, you should
-- not further inspect the Stream, and if you had sent a pipeline with multiple queries and you run this
-- function, you should not try to consume the results of any query in that pipeline.
-- Also, PostgreSQL's protocol specifies cancellation requests require opening a new connection to the
-- server, which means parallelism can introduce non-determinism, as such:
--
--     forkIO $ query conn "SELECT ..."
--     sendCancellationRequest conn
--
-- That the cancellation request _can_ arrive before the query even arrives, so it won't be cancelled,
-- and this _can_ happen even if all the messages of the "SELECT ..." query are sent first.
-- The cancellation request can also arrive after the active query finishes.
--
-- Modulo the race condition mentioned above, the database connection should be in a healthy
-- and usable state after this function returns.
cancelAnyRunningStatement :: HPgConnection -> IO ()
cancelAnyRunningStatement conn@HPgConnection {connOpts} = do
  -- Drain results of orphaned queries if necessary
  queriesToDrain <- acquireOwnershipOfOrphanedQueries
  -- Acquire control-msg lock when draining to avoid a race condition where
  -- soon after draining the last query a different thread runs a new query.
  -- We want the supplied `f` function to run on a clean state/pipeline.
  unless (null queriesToDrain) $ do
    debugPrint $ "Going to take control-msg lock to drain " ++ show queriesToDrain
    withControlMsgsLock conn (const $ pure ()) (const $ pure ()) $ const $ do
      -- It is possible not just in theory for the cancellation request to
      -- arrive/be processed by postgres _before_ the previous statement was
      -- event sent, since cancellation requests go through a different
      -- connection, so there's no guarantee of delivery in order.
      -- Even if the cancellation request arrives later at the server machine,
      -- the kernel can still deliver them in different order, and postgres
      -- itself can process them in different order, at least due to the
      -- kernel's scheduler not giving guarantees.
      -- We've seen it happen in our tests (i.e. "Exercise interruption safety"),
      -- so this is not merely hypothetical.
      -- What we do here is fire a cancellation request every 0.5 seconds to cover
      -- for that.
      debugPrint "Cancelling active statement to drain"
      sendCancellationRequest conn
      -- putStrLn $ "Draining " ++ show queriesToDrain
      -- debugPrint $ "Draining " ++ show queriesToDrain
      let drainUntilError [] = pure ()
          drainUntilError (q : qs) = do
            (_, res) <- consumeResults conn q
            eErrorOrCmdComplete <- S.effects res
            -- If we get an error, we cannot continue to consume the results
            -- of other queries as the whole pipeline is trashed
            case eErrorOrCmdComplete of
              Right _cmdComplete -> drainUntilError qs
              -- Left _err -> putStrLn "Got error, stopping draining" >> pure ()
              Left _err -> pure ()
          alternateDrainingWithCancelReqs qs = do
            drained <- timeout (1000 * cancellationRequestResendIntervalMs connOpts) $ drainUntilError qs
            case drained of
              Just () -> pure ()
              Nothing -> do
                debugPrint $ "Sending another cancellation request as orphaned pipeline still not completely drained: " ++ show queriesToDrain
                -- putStrLn $ "Sending another cancellation request as orphaned pipeline still not completely drained: " ++ show queriesToDrain
                sendCancellationRequest conn
                leftToDrain <- acquireOwnershipOfOrphanedQueries
                alternateDrainingWithCancelReqs leftToDrain
      alternateDrainingWithCancelReqs queriesToDrain
  where
    -- \| Returns queries that have been taken possession of by this thread for cancellation and draining
    -- or an empty list if there's no need for that.
    acquireOwnershipOfOrphanedQueries :: IO [QueryId]
    acquireOwnershipOfOrphanedQueries = do
      thisThreadId <- getMyWeakThreadId
      debugPrint $ "+++ I am " ++ show thisThreadId ++ " and will look for orphaned queries to drain"
      withControlMsgsLock
        conn
        STM.readTVar
        (const $ pure ())
        $ \st -> do
          if isRight (connectionReadyForNewPipeline st)
            then pure []
            else do
              -- TODO: We should either move the WeakThreadId owner into the full pipeline,
              -- or change this to a `takeWhile` because the internal model allows different
              -- queries to have different owners, even if in practice that shouldn't happen.
              let activeQueries = currentPipeline st
              mustTakeOwnership <- fmap (List.foldl' (||) False) $ forM activeQueries $ \QueryState {queryOwner} ->
                -- See Note [`timeout` uses the same ThreadId] for why having the same ThreadId _still_ means
                -- we need to cancel and drain those queries
                if queryOwner == thisThreadId then pure True else threadDoesNotExist queryOwner
              if mustTakeOwnership
                then do
                  STM.atomically $ STM.writeTVar (internalConnectionState conn) $ st {currentPipeline = map (\qs -> qs {queryOwner = thisThreadId}) $ currentPipeline st}
                  let owner = map queryOwner activeQueries
                  debugPrint $ "We (" ++ show thisThreadId ++ ") took ownership of the pipeline orphaned by " ++ show owner
                  -- putStrLn $ "We (" ++ show thisThreadId ++ ") took ownership of the pipeline orphaned by " ++ show owner
                  pure $ map queryIdentifier activeQueries
                else pure []
    threadDoesNotExist :: WeakThreadId -> IO Bool
    threadDoesNotExist (WeakThreadId wtid _) =
      deRefWeak wtid >>= \case
        Nothing -> pure True
        Just tid -> (`elem` [ThreadDied, ThreadFinished]) <$> threadStatus tid

pipelineS :: RowParser a -> Query -> Pipeline (IO (Stream (Of a) IO ()))
pipelineS rowparser@(RowParser _ _ expectedColFmts) (lastAndInitNE . breakQueryIntoStatements -> (firstQueriesToSend, lastQueryToSend)) =
  Pipeline
    (map (,Nothing) firstQueriesToSend ++ [(lastQueryToSend, Just expectedColFmts)])
    ( \conn qryIds -> do
        case lastAndInit qryIds of
          (firstQueries, mLastQry) -> do
            forM_ firstQueries $ consumeResultsIgnoreRows conn
            pure $ consumeStreamingResults rowparser conn (fromMaybe (error "pipelineS internal bug: no mLastQry") mLastQry)
    )

pipelineL :: RowParser a -> Query -> Pipeline (IO [a])
pipelineL rowparser q = (S.toList_ =<<) <$> pipelineS rowparser q

pipelineCmd :: Query -> Pipeline (IO Int64)
pipelineCmd = pipelineCmdInternal . breakQueryIntoStatements

pipelineCmdInternal :: NonEmpty SingleQuery -> Pipeline (IO Int64)
pipelineCmdInternal qs =
  Pipeline
    (map (,Nothing) (NE.toList qs))
    ( \conn qryIds -> do
        case lastAndInit qryIds of
          (firstQueries, mLastQry) -> do
            forM_ firstQueries $ consumeResultsIgnoreRows conn
            consumeResultsIgnoreRows conn (fromMaybe (error "pipelineCmd internal bug: no mLastQry") mLastQry)
    )

-- | Runs a pipeline of statements, that is, sends multiple SQL statements in a single round-trip
-- to the server.
-- Note that the thread that runs this must be the thread that consumes the
-- results of every query in the supplied pipeline, and in order.
-- Anything else is not officially supported by HPgsql and may result in deadlocks or undefined behaviour.
runPipeline :: HPgConnection -> Pipeline a -> IO a
runPipeline conn (Pipeline (NE.nonEmpty -> mQueries) run) =
  case mQueries of
    Nothing -> pure $ run conn []
    Just queries -> do
      encodingContext <- readMVar conn.encodingContext
      let toMessages (SingleQuery qryString qryParams, mExpectedResultColFmts) =
            let paramOidsAndValues = map ($ encodingContext) qryParams
             in [ SomeMessage $ Parse qryString (map fst paramOidsAndValues),
                  SomeMessage $
                    Bind
                      { paramsValuesInOrder = map snd paramOidsAndValues,
                        resultColumnFmts = fromMaybe [BinaryFmt] mExpectedResultColFmts
                      },
                  SomeMessage Describe,
                  SomeMessage Execute
                ]
      atomicallyInitiatePipelineOrPanicAndThenConsumeResults
        conn
        (fmap (\(SingleQuery {queryString}, _) -> (queryString, ExtendedQuery)) queries)
        (concatMap toMessages queries ++ [SomeMessage Sync])
        $ \qryIds -> pure $ run conn (NE.toList qryIds)

consumeStreamingResults :: RowParser a -> HPgConnection -> QueryId -> Stream (Of a) IO ()
consumeStreamingResults (RowParser rparser rtypecheck expectedColFmts) conn qryId = S.effect $ do
  qText <- lookupQueryText conn qryId
  (mERowDesc, rowsStream) <- consumeResults conn qryId
  case mERowDesc of
    Nothing -> do
      -- This is likely an error that happened when binding parameters (e.g. more/fewer params necessary than were sent)
      -- or a query that has no parameters and fails very early (e.g. "SELECT 1/0")
      rowCount :> res <- S.length rowsStream
      when (rowCount > 0) $ throwIrrecoverableErrorWithStatement qText "Bug in HPgsql. We didn't get either NoData or RowDescription, so we assumed there was an error binding the query, but we got more than 0 rows in results"
      case res of
        Left err -> throwPostgresError qText err
        Right _cmd -> throwIrrecoverableErrorWithStatement qText "Bug in HPgsql. We didn't get either NoData or RowDescription, so we assumed there was an error binding the query, but we then received a CommandComplete."
    Just (Left3 _noData) -> throwIrrecoverableErrorWithStatement qText "You have sent a count-returning query but expected it to be a rows-returning query, so we are aborting."
    Just (Right3 _copyInResponse) -> throwIrrecoverableErrorWithStatement qText "You have sent a COPY FROM STDIN query but expected it to be a rows-returning query, so we are aborting."
    Just (Middle3 (RowDescription coltypes)) -> do
      encodingContext <- readMVar conn.encodingContext
      let numResultColumns = length coltypes
          expectedNumCols = length expectedColFmts
          mkColInfo oid = ColumnInfo oid encodingContext
          colInfos = map mkColInfo coltypes
          typecheckedColInfos = rtypecheck colInfos
      unless (numResultColumns == expectedNumCols) $ throwIrrecoverableErrorWithStatement qText $ "Query result contains " ++ show numResultColumns ++ " columns but row parser expected " ++ show expectedNumCols
      unless (all snd typecheckedColInfos) $ throwIrrecoverableErrorWithStatement qText "Query result column types do not match expected column types"
      let !rowparser = rparser colInfos <* Parsec.endOfInput
      pure $ do
        errOrCmdComplete <-
          S.mapM
            ( \(DataRow rowColumnData) ->
                case LazyParsec.parseOnly rowparser rowColumnData of
                  Right row -> pure row
                  Left err -> throwIrrecoverableErrorWithStatement qText $ "Failed parsing a row: " ++ show err
            )
            rowsStream
        S.effect $ case errOrCmdComplete of
          Left err -> throwPostgresError qText err
          Right _cmdComplete -> pure mempty

query :: forall a. (FromPgRow a) => HPgConnection -> Query -> IO [a]
query = queryWith (rowParser @a)

queryWith :: RowParser a -> HPgConnection -> Query -> IO [a]
queryWith rparser conn qry = join $ runPipeline conn $ pipelineL rparser qry

withCopy :: HPgConnection -> Query -> IO () -> IO Int64
withCopy conn (lastAndInitNE . breakQueryIntoStatements -> (firstQueries, SingleQuery {..})) copyFn = do
  whenNonEmpty firstQueries $ \fqne -> runPipeline conn $ pipelineCmdInternal fqne
  thisThreadId <- getMyWeakThreadId
  encodingContext <- readMVar conn.encodingContext
  let paramOidsAndValues = map ($ encodingContext) queryParams
  atomicallyInitiatePipelineOrPanicAndThenConsumeResults
    conn
    ((queryString, CopyQuery StillCopying) :| [])
    [ SomeMessage $ Parse queryString (map fst paramOidsAndValues),
      SomeMessage $ Bind {paramsValuesInOrder = map snd paramOidsAndValues, resultColumnFmts = []},
      -- We don't send Msgs.Describe because we expect CopyInResponse in place of NoData
      SomeMessage Execute,
      SomeMessage Msgs.Flush -- This might not be necessary for COPY, but possibly useful if the user calls this with not-a-COPY statement so we get errors earlier?
    ]
    $ \(qryId :| _) -> do
      void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
      void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
      void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
      copyFn
      copyEndInternal conn qryId

copyStart :: HPgConnection -> Query -> IO ()
copyStart conn (lastAndInitNE . breakQueryIntoStatements -> (firstQueries, SingleQuery {..})) = do
  -- If the query contains not just COPY, but other statements before, we run
  -- those. This is awkward, but what else should we do? Throw?
  whenNonEmpty firstQueries $ \fqne -> runPipeline conn $ pipelineCmdInternal fqne
  thisThreadId <- getMyWeakThreadId
  encodingContext <- readMVar conn.encodingContext
  let paramOidsAndValues = map ($ encodingContext) queryParams
  atomicallyInitiatePipelineOrPanicAndThenConsumeResults
    conn
    ((queryString, CopyQuery StillCopying) :| [])
    [ SomeMessage $ Parse queryString (map fst paramOidsAndValues),
      SomeMessage $ Bind {paramsValuesInOrder = map snd paramOidsAndValues, resultColumnFmts = []},
      -- We don't send Msgs.Describe because we expect CopyInResponse in place of NoData
      SomeMessage Execute,
      SomeMessage Msgs.Flush -- This might not be necessary for COPY, but possibly useful if the user calls this with not-a-COPY statement so we get errors earlier?
    ]
    $ \(qryId :| _) -> do
      -- TODO: If this part is interrupted, I doubt Hpgsql can resume execution
      -- sanely! Maybe at least for now we could detect an async exception and
      -- rethrow an irrecoverable error? It could still go unnoticed in a
      -- dead thread, though.
      void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
      void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId
      void $ receiveOutstandingResponseMsgsAtomically thisThreadId conn qryId

putCopyData :: HPgConnection -> ByteString -> IO ()
putCopyData conn t = nonAtomicSendMsg conn (CopyData t)

copyEnd :: HPgConnection -> IO Int64
copyEnd conn = do
  thisThreadId <- getMyWeakThreadId
  qryId <- STM.atomically $ do
    st <- STM.readTVar conn.internalConnectionState
    case st.currentPipeline of
      [] -> throwIrrecoverableErrorWithStatement "Ending a COPY statement" "No active COPY statement when running copyEnd"
      [qs] -> case qs.queryProtocol of
        ExtendedQuery -> throwIrrecoverableErrorWithStatement "Ending a COPY statement" "No active COPY statement when running copyEnd, but rather there was a regular query"
        CopyQuery StillCopying -> if qs.queryOwner == thisThreadId then pure qs.queryIdentifier else throwIrrecoverableErrorWithStatement "Ending a COPY statement" "Active COPY statement was issued by a different thread, and HPgsql does not support multiple threads running/ending the same COPY statement"
        CopyQuery CopyDoneAndSyncSent -> throwIrrecoverableErrorWithStatement "Ending a COPY statement" "Active COPY statement was already finished"
        CopyQuery CopyFailAndSyncSent -> throwIrrecoverableErrorWithStatement "Ending a COPY statement" "Active COPY statement had previously failed"
      _ -> throwIrrecoverableErrorWithStatement "Ending a COPY statement" "Active pipeline with other statements running when a copyEnd was attempted"

  copyEndInternal conn qryId

copyEndInternal :: HPgConnection -> QueryId -> IO Int64
copyEndInternal conn qryId = do
  atomicallySendControlMsgs_
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
  qText <- lookupQueryText conn qryId
  (_, stream) <- consumeResults conn qryId
  res <- S.effects stream
  case res of
    Left err -> throwPostgresError qText err
    Right (CommandComplete n) -> pure n

-- | A simpler version of `atomicallySendControlMsgs`.
atomicallySendControlMsgs_ :: HPgConnection -> ([SomeMessage], STM ()) -> IO ()
atomicallySendControlMsgs_ conn (msgs, stateUpdate) = atomicallySendControlMsgs conn (const $ pure ()) (const $ pure ()) (const ((), msgs, stateUpdate))

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
atomicallySendControlMsgs :: HPgConnection -> (TVar InternalConnectionState -> STM a) -> (TVar InternalConnectionState -> STM ()) -> (a -> (b, [SomeMessage], STM ())) -> IO b
atomicallySendControlMsgs conn acquire release f = do
  withControlMsgsLock
    conn
    acquire
    release
    $ \acqValue ->
      let (!ret, !msgs, !afterSentTxn) = f acqValue
       in modifyMVar (sendBuffer conn) $ \msgsInBuffer ->
            -- Use a DList for appends? Probably not worth it since there are so few control messages.
            pure (msgsInBuffer ++ [(Builder.toLazyByteString (mconcat $ map toPgMessage msgs), afterSentTxn)], ret)

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
    waitUntilPipelineIsReadyForNewQuery conn (pure ()) $ \() ->
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

-- | Non-atomic because if it's interrupted, it can leave the socket in a state
-- where only part of the message's bytes were pushed across the userspace<->kernel
-- boundary, leaving the connection in a ruined state.
-- TODO: Maybe we should mark the connection as broken on exception?
nonAtomicSendMsg :: (ToPgMessage msg, Show msg) => HPgConnection -> msg -> IO ()
nonAtomicSendMsg HPgConnection {socket} msg = do
  SocketLBS.sendAll socket $ Builder.toLazyByteString $ toPgMessage msg
  debugPrint $ "Sent " ++ show msg

-- | Wraps an IO action to rethrow HPgsql's 'PostgresError' as postgresql-simple's 'SqlError'.
rethrowAsIrrecoverable :: IO a -> IO a
rethrowAsIrrecoverable = handle (throw . asIrrec)
  where
    asIrrec ex = IrrecoverableHpgsqlError {hpgsqlDetails = "An inner exception was thrown", pgErrorDetails = mempty, innerException = Just ex, relatedStatement = Nothing}

{-# NOINLINE _globalDebugLock #-}
_globalDebugLock :: MVar Bool
_globalDebugLock = unsafePerformIO $ newMVar True

debugPrint :: String -> IO ()
debugPrint _ = pure ()

-- debugPrint str = modifyMVar_ _globalDebugLock $ \p -> when p (putStrLn str) >> pure p

{-# INLINE timeDebugNonBlockingOperation #-}
timeDebugNonBlockingOperation :: String -> IO a -> IO a
timeDebugNonBlockingOperation _ f = f

-- timeDebugNonBlockingOperation opName f = do
--   t1 <- getMonotonicTime
--   ret <- f
--   t2 <- getMonotonicTime
--   when (t2 - t1 > 0.01) $ putStrLn $ opName ++ " took more than 10ms: " ++ show (t2 - t1)
--   pure ret

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
