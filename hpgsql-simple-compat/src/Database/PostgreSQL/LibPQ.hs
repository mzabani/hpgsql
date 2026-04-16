module Database.PostgreSQL.LibPQ (fname, transactionStatus, Column (..), Connection (..), ExecStatus (..), Format (..), Oid (..), Result (..), Row (..), TransactionStatus (..)) where

import Data.ByteString (ByteString)
import qualified Hpgsql
import Hpgsql.TypeInfo (Oid (..), TransactionStatus (..))

data Connection = Connection {hpgConn :: Hpgsql.HPgConnection}

data ExecStatus = FatalError
  deriving (Eq, Show)

data Result = Result

data Column = Column

data Row = Row

data Format = Text | Binary

fname :: Result -> Column -> IO (Maybe ByteString)
fname _ _ = pure Nothing

transactionStatus :: Connection -> IO TransactionStatus
transactionStatus conn = Hpgsql.transactionStatus $ hpgConn conn
