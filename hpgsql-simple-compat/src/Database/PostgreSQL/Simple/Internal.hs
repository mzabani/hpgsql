{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.Internal
-- Copyright:   (c) 2011-2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Internal bits.  This interface is less stable and can change at any time.
-- In particular this means that while the rest of the hpgsql-simple-compat
-- package endeavors to follow the package versioning policy,  this module
-- does not.  Also, at the moment there are things in here that aren't
-- particularly internal and are exported elsewhere;  these will eventually
-- disappear from this module.
module Database.PostgreSQL.Simple.Internal where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (MonadPlus (..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import Data.Int (Int64)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Data.Word
import Database.PostgreSQL.LibPQ (ExecStatus (..), Oid (..))
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Compat (toByteString)
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.ToField (Action (..), inQuotes)
import Database.PostgreSQL.Simple.TypeInfo.Types (TypeInfo)
import Database.PostgreSQL.Simple.Types (Query (..))
import GHC.Generics
import GHC.IO.Exception
import HPgsql (HPgConnection)
import qualified HPgsql
import qualified HPgsql.Connection
#if !defined(mingw32_HOST_OS)
import           Control.Concurrent(threadWaitRead, threadWaitWrite)
#endif

-- | A Field represents metadata about a particular field
--
-- You don't particularly want to retain these structures for a long
-- period of time,  as they will retain the entire query result,  not
-- just the field metadata
data Field = Field
  { result :: !PQ.Result,
    column :: {-# UNPACK #-} !PQ.Column,
    -- | This returns the type oid associated with the column.  Analogous
    --   to libpq's @PQftype@.
    typeOid :: {-# UNPACK #-} !PQ.Oid
  }

type TypeInfoCache = IntMap.IntMap TypeInfo

data Connection = Connection
  { connectionObjects :: {-# UNPACK #-} !(MVar TypeInfoCache),
    connectionTempNameCounter :: {-# UNPACK #-} !(IORef Int64),
    hpgConn :: HPgConnection
  }
  deriving (Typeable)

instance Eq Connection where
  x == y = hpgConn x == hpgConn y

-- | Superclass for postgresql exceptions
data SomePostgreSqlException = forall e. (Exception e) => SomePostgreSqlException e
  deriving (Typeable)

postgresqlExceptionToException :: (Exception e) => e -> SomeException
postgresqlExceptionToException = toException . SomePostgreSqlException

postgresqlExceptionFromException :: (Exception e) => SomeException -> Maybe e
postgresqlExceptionFromException x = do
  SomePostgreSqlException a <- fromException x
  cast a

instance Show SomePostgreSqlException where
  showsPrec :: Int -> SomePostgreSqlException -> ShowS
  showsPrec p (SomePostgreSqlException e) = showsPrec p e

instance Exception SomePostgreSqlException where
  displayException (SomePostgreSqlException e) = displayException e

data SqlError = SqlError
  { sqlState :: ByteString,
    sqlExecStatus :: ExecStatus,
    sqlErrorMsg :: ByteString,
    sqlErrorDetail :: ByteString,
    sqlErrorHint :: ByteString
  }
  deriving (Eq, Show, Typeable)

fatalError :: ByteString -> SqlError
fatalError msg = SqlError "" FatalError msg "" ""

instance Exception SqlError where
  toException = postgresqlExceptionToException
  fromException = postgresqlExceptionFromException

-- | Exception thrown if 'query' is used to perform an @INSERT@-like
-- operation, or 'execute' is used to perform a @SELECT@-like operation.
data QueryError = QueryError
  { qeMessage :: String,
    qeQuery :: Query
  }
  deriving (Eq, Show, Typeable)

instance Exception QueryError where
  toException = postgresqlExceptionToException
  fromException = postgresqlExceptionFromException

-- | Exception thrown if a 'Query' could not be formatted correctly.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError
  { fmtMessage :: String,
    fmtQuery :: Query,
    fmtParams :: [ByteString]
  }
  deriving (Eq, Show, Typeable)

instance Exception FormatError where
  toException = postgresqlExceptionToException
  fromException = postgresqlExceptionFromException

data ConnectInfo = ConnectInfo
  { connectHost :: String,
    connectPort :: Word16,
    connectUser :: String,
    connectPassword :: String,
    connectDatabase :: String
  }
  deriving (Generic, Eq, Read, Show, Typeable)

-- | Default information for setting up a connection.
--
-- Defaults are as follows:
--
-- * Server on @localhost@
--
-- * Port on @5432@
--
-- * User @postgres@
--
-- * No password
--
-- * Database @postgres@
--
-- Use as in the following example:
--
-- > connect defaultConnectInfo { connectHost = "db.example.com" }
defaultConnectInfo :: ConnectInfo
defaultConnectInfo =
  ConnectInfo
    { connectHost = "127.0.0.1",
      connectPort = 5432,
      connectUser = "postgres",
      connectPassword = "",
      connectDatabase = ""
    }

-- | Connect with the given username to the given database. Will throw
--   an exception if it cannot connect.
connect :: ConnectInfo -> IO Connection
connect = connectPostgreSQL . postgreSQLConnectionString

-- | Memory bracket around 'connect' and 'close'.
--
-- @since 0.6.5
withConnect :: ConnectInfo -> (Connection -> IO c) -> IO c
withConnect connInfo = bracket (connect connInfo) close

-- | Attempt to make a connection based on a libpq connection string.
--   See <https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING>
--   for more information.  Also note that environment variables also affect
--   parameters not provided, parameters provided as the empty string, and a
--   few other things; see
--   <https://www.postgresql.org/docs/9.5/static/libpq-envars.html>
--   for details.  Here is an example with some of the most commonly used
--   parameters:
--
-- > host='db.somedomain.com' port=5432 ...
--
--   This attempts to connect to @db.somedomain.com:5432@.  Omitting the port
--   will normally default to 5432.
--
--   On systems that provide unix domain sockets,  omitting the host parameter
--   will cause libpq to attempt to connect via unix domain sockets.
--   The default filesystem path to the socket is constructed from the
--   port number and the @DEFAULT_PGSOCKET_DIR@ constant defined in the
--   @pg_config_manual.h@ header file.  Connecting via unix sockets tends
--   to use the @peer@ authentication method, which is very secure and
--   does not require a password.
--
--   On Windows and other systems without unix domain sockets, omitting
--   the host will default to @localhost@.
--
-- > ... dbname='postgres' user='postgres' password='secret \' \\ pw'
--
--   This attempts to connect to a database named @postgres@ with
--   user @postgres@ and password @secret \' \\ pw@.  Backslash
--   characters will have to be double-quoted in literal Haskell strings,
--   of course.  Omitting @dbname@ and @user@ will both default to the
--   system username that the client process is running as.
--
--   Omitting @password@ will default to an appropriate password found
--   in the @pgpass@ file,  or no password at all if a matching line is
--   not found.  The path of the @pgpass@ file may be specified by setting
--   the @PGPASSFILE@ environment variable. See
--   <https://www.postgresql.org/docs/9.5/static/libpq-pgpass.html> for
--   more information regarding this file.
--
--   As all parameters are optional and the defaults are sensible,  the
--   empty connection string can be useful for development and
--   exploratory use,  assuming your system is set up appropriately.
--
--   On Unix,  such a setup would typically consist of a local
--   postgresql server listening on port 5432,  as well as a system user,
--   database user, and database sharing a common name,  with permissions
--   granted to the user on the database.
--
--   On Windows,  in addition you will either need @pg_hba.conf@
--   to specify the use of the @trust@ authentication method for
--   the connection,  which may not be appropriate for multiuser
--   or production machines, or you will need to use a @pgpass@ file
--   with the @password@ or @md5@ authentication methods.
--
--   See <https://www.postgresql.org/docs/9.5/static/client-authentication.html>
--   for more information regarding the authentication process.
--
--   SSL/TLS will typically "just work" if your postgresql server supports or
--   requires it.  However,  note that libpq is trivially vulnerable to a MITM
--   attack without setting additional SSL connection parameters.  In
--   particular,  @sslmode@ needs to be set to @require@, @verify-ca@, or
--   @verify-full@ in order to perform certificate validation.  When @sslmode@
--   is @require@,  then you will also need to specify a @sslrootcert@ file,
--   otherwise no validation of the server's identity will be performed.
--   Client authentication via certificates is also possible via the
--   @sslcert@ and @sslkey@ parameters.   See
--   <https://www.postgresql.org/docs/9.5/static/libpq-ssl.html>
--   for detailed information regarding libpq and SSL.
connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connstr = do
  connectionObjects <- newMVar (IntMap.empty)
  connectionTempNameCounter <- newIORef 0
  case HPgsql.Connection.parseConnString (TE.decodeUtf8 connstr) of
    Left err -> error err
    Right connStr -> do
      hpgConn <- HPgsql.connect connStr 30
      pure $ Connection {..}

connectdb :: ByteString -> IO PQ.Connection
#if defined(mingw32_HOST_OS)
connectdb = PQ.connectdb
#else
connectdb conninfo = do
    conn <- PQ.connectStart conninfo
    loop conn
  where
    funcName = "Database.PostgreSQL.Simple.connectPostgreSQL"
    loop conn = do
      status <- PQ.connectPoll conn
      case status of
        PQ.PollingFailed  -> throwLibPQError conn "connection failed"
        PQ.PollingReading -> do
                                mfd <- PQ.socket conn
                                case mfd of
                                  Nothing -> throwIO $! fdError funcName
                                  Just fd -> do
                                      threadWaitRead fd
                                      loop conn
        PQ.PollingWriting -> do
                                mfd <- PQ.socket conn
                                case mfd of
                                  Nothing -> throwIO $! fdError funcName
                                  Just fd -> do
                                      threadWaitWrite fd
                                      loop conn
        PQ.PollingOk      -> return conn

#endif

-- | Turns a 'ConnectInfo' data structure into a libpq connection string.
postgreSQLConnectionString :: ConnectInfo -> ByteString
postgreSQLConnectionString connectInfo = fromString connstr
  where
    connstr =
      str "host=" connectHost $
        num "port=" connectPort $
          str "user=" connectUser $
            str "password=" connectPassword $
              str "dbname=" connectDatabase $
                []

    str name field
      | null value = id
      | otherwise = showString name . addQuotes value . space
      where
        value = field connectInfo

    num name field
      | value <= 0 = id
      | otherwise = showString name . shows value . space
      where
        value = field connectInfo

    addQuotes s rest = '\'' : foldr delta ('\'' : rest) s
      where
        delta c cs = case c of
          '\\' -> '\\' : '\\' : cs
          '\'' -> '\\' : '\'' : cs
          _ -> c : cs

    space [] = []
    space xs = ' ' : xs

oid2int :: Oid -> Int
oid2int (Oid x) = fromIntegral x
{-# INLINE oid2int #-}

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO Int64
execute_ conn (Query stmt) = HPgsql.execute (hpgConn conn) (fromString $ T.unpack $ TE.decodeUtf8 stmt)

throwResultError :: ByteString -> PQ.Result -> PQ.ExecStatus -> IO a
throwResultError _ result status = do
  errormsg <-
    fromMaybe ""
      <$> PQ.resultErrorField result PQ.DiagMessagePrimary
  detail <-
    fromMaybe ""
      <$> PQ.resultErrorField result PQ.DiagMessageDetail
  hint <-
    fromMaybe ""
      <$> PQ.resultErrorField result PQ.DiagMessageHint
  state' <- maybe "" id <$> PQ.resultErrorField result PQ.DiagSqlstate
  throwIO $
    SqlError
      { sqlState = state',
        sqlExecStatus = status,
        sqlErrorMsg = errormsg,
        sqlErrorDetail = detail,
        sqlErrorHint = hint
      }

disconnectedError :: SqlError
disconnectedError = fatalError "connection disconnected"

close :: Connection -> IO ()
close Connection {..} = HPgsql.closeGracefully hpgConn

data Row = Row
  { row :: {-# UNPACK #-} !PQ.Row,
    rowresult :: !PQ.Result
  }

newtype RowParser a = RP {unRP :: ReaderT Row (StateT PQ.Column Conversion) a}
  deriving (Functor, Applicative, Alternative, Monad)

liftRowParser :: IO a -> RowParser a
liftRowParser = RP . lift . lift . liftConversion

newtype Conversion a = Conversion {runConversion :: Connection -> IO (Ok a)}

liftConversion :: IO a -> Conversion a
liftConversion m = Conversion (\_ -> Ok <$> m)

instance Functor Conversion where
  fmap f m = Conversion $ \conn -> (fmap . fmap) f (runConversion m conn)

instance Applicative Conversion where
  pure a = Conversion $ \_conn -> pure (pure a)
  mf <*> ma = Conversion $ \conn -> do
    okf <- runConversion mf conn
    case okf of
      Ok f -> (fmap . fmap) f (runConversion ma conn)
      Errors errs -> return (Errors errs)

instance Alternative Conversion where
  empty = Conversion $ \_conn -> pure empty
  ma <|> mb = Conversion $ \conn -> do
    oka <- runConversion ma conn
    case oka of
      Ok _ -> return oka
      Errors _ -> (oka <|>) <$> runConversion mb conn

instance Monad Conversion where
  m >>= f = Conversion $ \conn -> do
    oka <- runConversion m conn
    case oka of
      Ok a -> runConversion (f a) conn
      Errors err -> return (Errors err)

instance MonadPlus Conversion where
  mzero = empty
  mplus = (<|>)

conversionMap :: (Ok a -> Ok b) -> Conversion a -> Conversion b
conversionMap f m = Conversion $ \conn -> f <$> runConversion m conn

conversionError :: (Exception err) => err -> Conversion a
conversionError err = Conversion $ \_ -> return (Errors [toException err])

newTempName :: Connection -> IO Query
newTempName Connection {..} = do
  !n <-
    atomicModifyIORef
      connectionTempNameCounter
      (\n -> let !n' = n + 1 in (n', n'))
  return $! Query $ B8.pack $ "temp" ++ show n

-- FIXME?  What error should getNotification and getCopyData throw?
fdError :: ByteString -> IOError
fdError funcName =
  IOError
    { ioe_handle = Nothing,
      ioe_type = ResourceVanished,
      ioe_location = B8.unpack funcName,
      ioe_description = "failed to fetch file descriptor",
      ioe_errno = Nothing,
      ioe_filename = Nothing
    }

libPQError :: ByteString -> IOError
libPQError desc =
  IOError
    { ioe_handle = Nothing,
      ioe_type = OtherError,
      ioe_location = "libpq",
      ioe_description = B8.unpack desc,
      ioe_errno = Nothing,
      ioe_filename = Nothing
    }

throwLibPQError :: PQ.Connection -> ByteString -> IO a
throwLibPQError conn default_desc = do
  msg <- maybe default_desc id <$> PQ.errorMessage conn
  throwIO $! libPQError msg

fmtError :: String -> Query -> [Action] -> a
fmtError msg q xs =
  throw
    FormatError
      { fmtMessage = msg,
        fmtQuery = q,
        fmtParams = map twiddle xs
      }
  where
    twiddle (Plain b) = toByteString b
    twiddle (Escape s) = s
    twiddle (EscapeByteA s) = s
    twiddle (EscapeIdentifier s) = s
    twiddle (Many ys) = B.concat (map twiddle ys)

fmtErrorBs :: Query -> [Action] -> ByteString -> a
fmtErrorBs q xs msg = fmtError (T.unpack $ TE.decodeUtf8 msg) q xs

-- | Quote bytestring or throw 'FormatError'
quote :: Query -> [Action] -> Either ByteString ByteString -> Builder
quote q xs = either (fmtErrorBs q xs) (inQuotes . byteString)

checkError :: PQ.Connection -> Maybe a -> IO (Either ByteString a)
checkError _ (Just x) = return $ Right x
checkError c Nothing = Left . maybe "" id <$> PQ.errorMessage c
