module HPgsql.InternalTypes
  ( -- * Simple types
    ConnString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    NotificationResponse (..),
    PoolCleanup (..),
    TransactionStatus (..),
    EncodingContext (..), -- re-exported from HPgsql.TypeInfo
    throwIrrecoverableError,

    -- * Msgs types (moved to avoid cycles)
    ParseComplete (..),
    BindComplete (..),
    NoData (..),
    RowDescription (..),
    CopyInResponse (..),
    ErrorResponse (..),
    CommandComplete (..),
    DataRow (..),
    ReadyForQuery (..),

    -- * Internal connection state types (moved to avoid cycles)
    InternalConnectionState (..),
    WeakThreadId (..),
    QueryState (..),
    QueryId (..),
    QueryProtocol (..),
    CopyQueryState (..),
    ResponseMsgsReceived (..),
    ResponseMsg (..),
    Either3 (..),

    -- * Connection and Pipeline types
    HPgConnection (..),
    Mutex (..),
    Pipeline (..),
    mkMutex,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (STM, TQueue, TVar)
import Control.Exception.Safe (Exception (..), MonadThrow, SomeException, throw)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import qualified Data.List as List
import Data.Map.Strict (Map)
import Data.Text (Text)
#if MIN_VERSION_base(4,19,0)
import Data.Word (Word16, Word64)
#else
import Data.Word (Word16)

#endif
import qualified Control.Concurrent.STM as STM
import HPgsql.Query (SingleQuery (..))
import HPgsql.TypeInfo (EncodingContext (..), Oid (..), TransactionStatus (..))
import Network.Socket (AddrInfo, Socket)
import System.Mem.Weak (Weak)

-- ------------------------------------------------------------------
-- Simple types
-- ------------------------------------------------------------------

-- TODO: Switch to Text as the default in record fields
data ConnString = ConnString
  { hostname :: String,
    port :: Word16,
    user :: String,
    password :: String,
    database :: String,
    options :: String
  }
  deriving stock (Eq)

instance Show ConnString where
  show _ = "ConnectionString"

data ConnectOpts = ConnectOpts
  { -- | How long in ms HPgsql will sleep before re-checking if active queries have been orphaned
    -- from their issuing threads having died. The default is 500ms, and this is only relevant
    -- if you plan on concurrently issuing queries on a single connection, and even then only
    -- if you expect your threads to be killed by asynchronous exceptions frequently enough,
    -- and you want resume using the connection and cannot wait ~500ms until HPgsql realizes
    -- it's fine to do so.
    -- You probably don't need to worry about this or tune it.
    killedThreadPollIntervalMs :: Int,
    -- | How long in ms HPgsql will wait before re-sending a cancellation request
    -- while draining orphaned queries (queries from dead threads). The default is 500ms,
    -- and this is only relevant if you plan on interrupting your queries with
    -- asynchronous exceptions, either by use of concurrency primitives or functions like
    -- `timeout`, and continue using the connection for new queries after that.
    cancellationRequestResendIntervalMs :: Int,
    -- | Immediately after connecting, run a query to fetch all types
    -- from the `pg_type` table. This makes them available in FromPgField
    -- instances.
    -- The default is True. You probably want to set it to False if you have
    -- a lot of custom types and are extremely latency sensitive.
    -- Note that builtin postgres types are available in the connection's type
    -- cache even if this is False, at no runtime cost.
    fillTypeInfoCache :: Bool
  }

data ErrorDetail
  = ErrorSeverity
  | ErrorCode
  | ErrorHumanReadableMsg
  | ErrorDetail
  | ErrorHint
  | ErrorPosition
  | ErrorInternalPosition
  | ErrorInternalCommand
  | ErrorContext
  | ErrorSchema
  | ErrorTable
  | ErrorColumn
  | ErrorType
  | ErrorConstraint
  | ErrorSourceFile
  | ErrorSourceLine
  | ErrorSourceRoutine
  deriving stock (Eq, Ord, Show)

data NotificationResponse = NotificationResponse {notifierPid :: !Int32, channelName :: !Text, notifPayload :: !Text}
  deriving stock (Eq, Show)

data PoolCleanup = PoolCleanup
  { -- | Runs `RESET ALL` and `RESET ROLE` on the connection. Defaults to True.
    resetAll :: Bool,
    -- | Runs `UNLISTEN *` on the connection and clears the internal queue of notifications. Defaults to True.
    unlistenAll :: Bool,
    -- | Throws an exception if there is an open transaction or if there's a transaction in error state. Defaults to True.
    checkTransactionState :: Bool
    -- TODO: Check for any temporary tables and throw?
  }

data PostgresError = PostgresError {pgErrorDetails :: Map ErrorDetail LBS.ByteString, failedStatement :: !ByteString}
  deriving stock (Show)

instance Exception PostgresError

-- | If you receive this exception, don't run any further SQL statements or use it for anything. Just close the connection with `closeForcefully` and discard it.
data IrrecoverableHpgsqlError = IrrecoverableHpgsqlError {hpgsqlDetails :: String, innerException :: Maybe SomeException, relatedStatement :: !(Maybe ByteString)}
  deriving stock (Show)

instance Exception IrrecoverableHpgsqlError

throwIrrecoverableError :: (MonadThrow m) => String -> m a
throwIrrecoverableError errMsg = throw $ IrrecoverableHpgsqlError {hpgsqlDetails = errMsg, innerException = Nothing, relatedStatement = Nothing}

-- ------------------------------------------------------------------
-- Msgs types (moved from HPgsql.Msgs to avoid cycles)
-- ------------------------------------------------------------------

data ParseComplete = ParseComplete
  deriving stock (Show)

data BindComplete = BindComplete
  deriving stock (Show)

data NoData = NoData
  deriving stock (Show)

newtype RowDescription = RowDescription {resultColumnTypes :: [Oid]}
  deriving stock (Show)

data CopyInResponse = CopyInResponse
  deriving stock (Show)

newtype ErrorResponse = ErrorResponse (Map ErrorDetail LBS.ByteString)
  deriving stock (Show)

newtype CommandComplete = CommandComplete {numRows :: Int64}
  deriving stock (Show)

newtype DataRow = DataRow {rowColumnData :: ByteString}

instance Show DataRow where
  show _ = "DataRow"

newtype ReadyForQuery
  = ReadyForQuery TransactionStatus
  deriving stock (Show)

-- ------------------------------------------------------------------
-- Internal connection state types (moved from HPgsql)
-- ------------------------------------------------------------------

data Either3 a b c = Left3 !a | Middle3 !b | Right3 !c
  deriving stock (Show)

-- | An Integer avoids any headaches from wrap-around when comparing query ids.
-- An Int64 should be fine, but the cost of this is negligible.
newtype QueryId = QueryId Integer
  deriving newtype (Enum, Eq, Num, Ord, Show)

data CopyQueryState = StillCopying | CopyDoneAndSyncSent | CopyFailAndSyncSent
  deriving stock (Eq, Show)

data QueryProtocol
  = CopyQuery CopyQueryState
  | ExtendedQuery
  deriving stock (Eq, Show)

-- | From the docs, "in GHC, if you have a ThreadId, you essentially have a pointer to the thread itself. This means the thread itself can't be garbage collected until you drop the ThreadId. This misfeature will hopefully be corrected at a later date.".
-- And as per https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Concurrent.html#v:mkWeakThreadId even BlockedIndefinitely exceptions aren't delivered if we held a ThreadId directly, so we only keep a WeakThreadId.
#if MIN_VERSION_base(4,19,0)
data WeakThreadId = WeakThreadId !(Weak ThreadId) !Word64
#else
-- fromThreadId is not available in GHC 9.6 and below, so
-- we rely on the String obtained from showThreadId instead
data WeakThreadId = WeakThreadId !(Weak ThreadId) !String
#endif

instance Eq WeakThreadId where
  WeakThreadId _ t1 == WeakThreadId _ t2 = t1 == t2

#if MIN_VERSION_base(4,19,0)
instance Show WeakThreadId where
  show (WeakThreadId _ threadIdentifier) = "WeakThreadId " ++ show threadIdentifier
#else
instance Show WeakThreadId where
  show (WeakThreadId _ threadIdentifier) = "WeakThreadId " ++ threadIdentifier
#endif

data ResponseMsgsReceived = NoMsgsReceived | ParseCompleteReceived ParseComplete | BindCompleteReceived BindComplete | RowDescriptionOrNoDataOrCopyInResponseReceived (Either3 NoData RowDescription CopyInResponse) | ErrorResponseReceived (Maybe (Either3 NoData RowDescription CopyInResponse)) ErrorResponse | CommandCompleteReceived (Either3 NoData RowDescription CopyInResponse) CommandComplete | ReadyForQueryReceived (Either ErrorResponse CommandComplete) ReadyForQuery
  deriving stock (Show)

data ResponseMsg = RespParseComplete ParseComplete | RespBindComplete BindComplete | RespNoData NoData | RespRowDescription RowDescription | RespCopyInResponse CopyInResponse | RespErrorResponse ErrorResponse | RespCommandComplete CommandComplete | RespDataRow DataRow | RespReadyForQuery ReadyForQuery
  deriving stock (Show)

data QueryState = QueryState
  { queryIdentifier :: !QueryId,
    queryText :: !ByteString,
    queryProtocol :: !QueryProtocol,
    queryOwner :: !WeakThreadId,
    -- | Storing every single "control" (i.e. not `DataRow`) message received for each query
    -- means we can continue to drain its results from another thread at anytime, which is
    -- necessary to continue using the connection in the presence of asynchronous exceptions.
    responseMsgsState :: ResponseMsgsReceived
  }
  deriving stock (Show)

data InternalConnectionState = InternalConnectionState
  { totalQueriesSent :: !Integer,
    -- | We support only one pipeline sent to the backend at a time.
    currentPipeline :: ![QueryState],
    notificationsReceived :: !(TQueue NotificationResponse),
    transactionStatusBeforeCurrentPipeline :: !TransactionStatus
  }

-- ------------------------------------------------------------------
-- Connection and Pipeline types
-- ------------------------------------------------------------------

data HPgConnection = HPgConnection
  { socket :: !Socket,
    socketClosed :: !(MVar Bool),
    recvBuffer :: !(MVar LBS.ByteString),
    sendBuffer :: !(MVar [(LBS.ByteString, STM ())]),
    socketMutex :: !Mutex,
    originalConnStr :: !ConnString,
    connectedTo :: !AddrInfo,
    encodingContext :: !(MVar EncodingContext),
    parameterStatusMap :: !(MVar (Map Text Text)),
    internalConnectionState :: !(TVar InternalConnectionState),
    connPid :: !Int32,
    cancelSecretKey :: !Int32,
    connOpts :: !ConnectOpts
  }

instance Eq HPgConnection where
  conn1 == conn2 = socket conn1 == socket conn2

-- | A reentrant mutex, i.e. one that can be re-acquired by the same
-- thread without deadlocking.
newtype Mutex = Mutex (TVar (Maybe (WeakThreadId, Int)))

mkMutex :: IO Mutex
mkMutex = Mutex <$> STM.newTVarIO Nothing

data Pipeline a = Pipeline [(SingleQuery, Maybe Int)] (HPgConnection -> [QueryId] -> a)
  deriving stock (Functor)

instance Applicative Pipeline where
  pure x = Pipeline [] (\_ _ -> x)
  Pipeline queries runFunc <*> Pipeline moreQueries run2 = Pipeline (queries ++ moreQueries) $ \conn qryIds ->
    let (firstQueries, lastQueries) = List.splitAt (length queries) qryIds
        f = runFunc conn firstQueries
        g = run2 conn lastQueries
     in f g
