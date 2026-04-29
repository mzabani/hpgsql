module Hpgsql.InternalTypes
  ( -- * Simple types
    ConnString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    NotificationResponse (..),
    PoolCleanup (..),
    TransactionStatus (..),
    EncodingContext (..), -- re-exported from Hpgsql.TypeInfo
    throwIrrecoverableError,

    -- * Query types (moved from Hpgsql.Query)
    SingleQueryFragment (..),
    Query (..),
    SingleQuery (..),
    queryToByteString,
    breakQueryIntoStatements,
    renumberParamsFrom,

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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
#if MIN_VERSION_base(4,19,0)
import Data.Word (Word16, Word64)
#else
import Data.Word (Word16)

#endif
import qualified Control.Concurrent.STM as STM
import Data.Hashable (hash)
import Data.Set (Set)
import Hpgsql.Base (lastTwoAndInit, maximumOnOrDef, minimumOnOrDef)
import Hpgsql.Builder (BinaryField)
import Hpgsql.Parsing (BlockOrNotBlock (..), ParsingOpts (..), parseSql)
import Hpgsql.TypeInfo (EncodingContext (..), Oid (..), TransactionStatus (..))
import Network.Socket (AddrInfo, Socket)
import System.Mem.Weak (Weak)

-- | A fragment of a single SQL statement, which is either static SQL or
-- a placeholder for a query argument.
data SingleQueryFragment
  = FragmentOfStaticSql !ByteString
  | FragmentWithSemiColon
  | FragmentOfCommentsOrWhitespace !ByteString
  | -- | The number/index of the query argument, starting from 1 in a single statement
    QueryArgumentPlaceHolder !Int
  deriving stock (Eq, Show)

-- | `Query` is some SQL with values of query arguments inside and also whether it's a prepared statement.
-- It can be concatenated to other `Query` objects freely, and concatenating chains of Queries with
-- at least one prepared statement among them makes the final `Query` object a prepared statement, too.
-- You can build this with the `sql` and `sqlPrep` quasiquoters, the former for unprepared statements,
-- and you can use `preparedStatement` and `nonPreparedStatement` to convert between them.
data Query = Query
  { --  The query parameters and query fragments continue to go up in number across different
    -- statements, e.g. "SELECT $1; SELECT $2; SELECT $3; ...".
    queryString :: ![SingleQueryFragment],
    queryParams :: ![EncodingContext -> (Maybe Oid, BinaryField)],
    isPrepared :: !Bool
  }

instance Show Query where
  show = show . NE.toList . breakQueryIntoStatements

queryToByteString :: Query -> ByteString
queryToByteString = mconcat . map (\SingleQuery {queryString} -> queryString) . NE.toList . breakQueryIntoStatements

instance Semigroup Query where
  q1 <> q2 =
    let maxArgQ1 =
          maximumOnOrDef
            0
            q1.queryString
            ( \case
                QueryArgumentPlaceHolder n -> Just n
                _ -> Nothing
            )
        (_, remappedQ2) =
          renumberParamsFrom
            q2.queryString
            (maxArgQ1 + 1)
     in -- Semigroup concatenation must be associative, and for isPrepared
        -- both || and && are associative, but what would the user expect?
        -- For typical query concatenation, I *think* of some examples:
        -- - [sqlPrep|SELECT ^{columnsQry} FROM some_table ^{whereConditions}|]
        -- - [sqlPrep|INSERT INTO x ^{vALUES rows}|]
        -- In both cases above, we want `isPrepared = True` to be "infectious", so
        -- we want to use ||
        -- Are there counter examples? Yes, if users build "smaller" embeddable Queries
        -- with `sqlPrep`, e.g. if `columnsQry` or `whereConditions` were built with `sqlPrep`.
        -- That feels unlikely.. so we go with ||
        Query {queryString = q1.queryString <> remappedQ2, queryParams = q1.queryParams <> q2.queryParams, isPrepared = q1.isPrepared || q2.isPrepared}

-- | A single statement, not multiple, with dollar-numbered query arguments
-- starting from $1.
data SingleQuery = SingleQuery {queryString :: !ByteString, queryParams :: ![EncodingContext -> (Maybe Oid, BinaryField)], preparedStmtHash :: !(Maybe String)}

mkSingleQuery :: ByteString -> [EncodingContext -> (Maybe Oid, BinaryField)] -> Bool -> SingleQuery
mkSingleQuery queryString queryParams isPrepared = SingleQuery {queryString, queryParams, preparedStmtHash = if isPrepared then Just (show $ hash queryString) else Nothing}

instance Show SingleQuery where
  -- Careful not exposing query arguments
  show (SingleQuery {queryString}) = show queryString

instance IsString Query where
  fromString s =
    let blocks = parseSql AcceptOnlyDollarNumberedArgs (Text.pack s)
        queryFrags =
          map
            ( \case
                StaticSql t -> FragmentOfStaticSql $ encodeUtf8 t
                SemiColon -> FragmentWithSemiColon
                CommentsOrWhitespace t -> FragmentOfCommentsOrWhitespace $ encodeUtf8 t
                DollarNumberedArg _ ->
                  error "Dollar-numbered query arguments are not supported in string literals. Use the sql quasiquoter or mkQuery instead."
                QuestionMarkArg ->
                  error "Question mark query arguments are not supported in string literals. Use the sql quasiquoter or mkQuery instead."
                QuasiQuoterExpression _ _ ->
                  error "Bug in Hpgsql: parseSql AcceptOnlyDollarNumberedArgs returned quasiquoter expression."
            )
            blocks
     in Query {queryString = queryFrags, queryParams = [], isPrepared = False}

-- | For internal usage only. Takes a `Query` and breaks it up into
-- individual SQL statements that can be sent to a postgres backend.
breakQueryIntoStatements :: Query -> NonEmpty SingleQuery
breakQueryIntoStatements qry@Query {queryString = fullQueryString, queryParams = allQueryParams, isPrepared} =
  -- If a user insists in trying to run an empty query, or if
  -- they mistakenly concatenate empty statements, we let them
  toEmptyQueryIfNecessary $
    map toSingleQuery $
      -- If a query string is "SELECT 1; -- comments and empty space", we put
      -- all comments and whitespace together with that last "real" SQL statement
      fixLastEmptyStatement $
        go fullQueryString allQueryParams
  where
    toEmptyQueryIfNecessary [] = NE.singleton $ mkSingleQuery "" allQueryParams isPrepared
    toEmptyQueryIfNecessary (x : xs) = x :| xs
    toSingleQuery (blks, prms) = let queryString = mconcat $ map fragToBytestring blks in mkSingleQuery queryString prms isPrepared
    allWhitespaceOrComments =
      all
        ( \case
            FragmentOfCommentsOrWhitespace _ -> True
            _ -> False
        )
    fixLastEmptyStatement :: [([SingleQueryFragment], [EncodingContext -> (Maybe Oid, BinaryField)])] -> [([SingleQueryFragment], [EncodingContext -> (Maybe Oid, BinaryField)])]
    fixLastEmptyStatement indivStmts = case lastTwoAndInit indivStmts of
      (_, Nothing) -> indivStmts -- Only 0 or 1 statements found
      (firstStmts, Just (secLst, lst))
        | allWhitespaceOrComments (fst lst) -> firstStmts ++ [secLst <> lst]
        | otherwise -> indivStmts
    isLastFragmentOfAStatement = \case
      FragmentWithSemiColon -> True
      _ -> False
    fragToBytestring = \case
      QueryArgumentPlaceHolder n -> "$" <> intToBs n
      FragmentOfStaticSql t -> t
      FragmentWithSemiColon -> ";"
      FragmentOfCommentsOrWhitespace t -> t
    go :: [SingleQueryFragment] -> [EncodingContext -> (Maybe Oid, BinaryField)] -> [([SingleQueryFragment], [EncodingContext -> (Maybe Oid, BinaryField)])]
    go [] [] = []
    go [] _ = error $ "Hpgsql error: empty query fragment list but outstanding query params. Number of query arguments is " ++ show (length allQueryParams) ++ " and query is " ++ show qry
    go frags params =
      let (stmtFrags, nextFrags) = case List.break isLastFragmentOfAStatement frags of
            (firstStmts, []) -> (firstStmts, [])
            (firstStmts, semiColon : next) -> (firstStmts ++ [semiColon], next)
          (maxArgNum, thisQueryFrags) = renumberParamsFrom stmtFrags 1
          (thisQueryParams, nextParams) = List.splitAt maxArgNum params
       in (thisQueryFrags, thisQueryParams) : go nextFrags nextParams

intToBs :: Int -> ByteString
intToBs = encodeUtf8 . Text.pack . show

-- | Returns new fragments remapped with `renumberFrom` as the smallest query argument number,
-- and also returns the maximum (new) query argument number, or 0 if there were no query arguments.
renumberParamsFrom :: [SingleQueryFragment] -> Int -> (Int, [SingleQueryFragment])
renumberParamsFrom frags renumberFrom =
  List.mapAccumR
    ( \(!maxSoFar) -> \case
        QueryArgumentPlaceHolder n -> let newNum = n - smallestArgNum + renumberFrom in (max newNum maxSoFar, QueryArgumentPlaceHolder newNum)
        x -> (maxSoFar, x)
    )
    0
    frags
  where
    smallestArgNum =
      minimumOnOrDef
        1
        frags
        ( \case
            QueryArgumentPlaceHolder n -> Just n
            _ -> Nothing
        )

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
  { -- | How long in ms Hpgsql will sleep before re-checking if active queries have been orphaned
    -- from their issuing threads having died. The default is 500ms, and this is only relevant
    -- if you plan on concurrently issuing queries on a single connection, and even then only
    -- if you expect your threads to be killed by asynchronous exceptions frequently enough,
    -- and you want resume using the connection and cannot wait ~500ms until Hpgsql realizes
    -- it's fine to do so.
    -- You probably don't need to worry about this or tune it.
    killedThreadPollIntervalMs :: Int,
    -- | How long in ms Hpgsql will wait before re-sending a cancellation request
    -- while draining orphaned queries (queries from dead threads). The default is 500ms,
    -- and this is only relevant if you plan on interrupting your queries with
    -- asynchronous exceptions, either by use of concurrency primitives or functions like
    -- `timeout`, and continue using the connection for new queries after that.
    -- It is not recommend setting this below 100ms, because orphaned query draining
    -- alternates with resending cancellation requests, so if this is too low it is possible
    -- that draining never finishes, leading to a form of livelock.
    cancellationRequestResendIntervalMs :: Int,
    -- | Immediately after connecting, run a query to fetch all types
    -- from the `pg_type` table. This makes them available in FromPgField
    -- instances.
    -- The default is True. You should only set it to False if you really
    -- know what you're doing, because class instances of custom types
    -- can stop working.
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
-- Msgs types (moved from Hpgsql.Msgs to avoid cycles)
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
-- Internal connection state types (moved from Hpgsql)
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
    queryPrepStmtName :: !(Maybe String),
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
    -- | In cases like when an asynchronous exception interrupts a `withTransaction`
    -- section, we want a "ROLLBACK" to run _before_ any next commands, to preserve
    -- the invariant that `withTransaction` ensures the started transaction is no more
    -- regardless of what happens.
    -- When such a thing happens, this field is True.
    mustIssueRollbackBeforeNextCommand :: Bool,
    notificationsReceived :: !(TQueue NotificationResponse),
    preparedStatementNames :: Set String,
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
