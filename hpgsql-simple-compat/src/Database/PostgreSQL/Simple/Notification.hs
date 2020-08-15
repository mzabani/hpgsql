{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Database.PostgreSQL.Simple.Notification
-- Copyright   :  (c) 2011-2015 Leon P Smith
--                (c) 2012 Joey Adams
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
--
-- Support for receiving asynchronous notifications via PostgreSQL's
-- Listen/Notify mechanism.  See
-- <https://www.postgresql.org/docs/9.5/static/sql-notify.html> for more
-- information.
--
-- Note that on Windows,  @getNotification@ currently uses a polling loop
-- of 1 second to check for more notifications,  due to some inadequacies
-- in GHC's IO implementation and interface on that platform.   See GHC
-- issue #7353 for more information.  While this workaround is less than
-- ideal,  notifications are still better than polling the database directly.
-- Notifications do not create any extra work for the backend,  and are
-- likely cheaper on the client side as well.
--
-- <https://hackage.haskell.org/trac/ghc/ticket/7353>
module Database.PostgreSQL.Simple.Notification
  ( Notification (..),
    getNotification,
    getNotificationNonBlocking,
    getBackendPID,
  )
where

import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple.Internal
import qualified HPgsql
import System.Posix.Types (CPid)

data Notification = Notification
  { notificationPid :: {-# UNPACK #-} !CPid,
    notificationChannel :: {-# UNPACK #-} !B.ByteString,
    notificationData :: {-# UNPACK #-} !B.ByteString
  }
  deriving (Show, Eq)

-- | Returns a single notification.   If no notifications are available,
--   'getNotification' blocks until one arrives.
--
--   It is safe to call 'getNotification' on a connection that is concurrently
--   being used for other purposes,   note however that PostgreSQL does not
--   deliver notifications while a connection is inside a transaction.
getNotification :: Connection -> IO Notification
getNotification conn = fromHpgsqlNotification <$> HPgsql.getNotification (hpgConn conn)

-- | Non-blocking variant of 'getNotification'.   Returns a single notification,
-- if available.   If no notifications are available,  returns 'Nothing'.
getNotificationNonBlocking :: Connection -> IO (Maybe Notification)
getNotificationNonBlocking conn = fmap fromHpgsqlNotification <$> HPgsql.getNotificationNonBlocking (hpgConn conn)

fromHpgsqlNotification :: HPgsql.NotificationResponse -> Notification
fromHpgsqlNotification HPgsql.NotificationResponse {..} =
  Notification
    { notificationPid = fromIntegral notifierPid,
      notificationChannel = encodeUtf8 channelName,
      notificationData = encodeUtf8 notifPayload
    }

-- | Returns the process 'CPid' of the backend server process
-- handling this connection.
--
-- The backend PID is useful for debugging purposes and for comparison
-- to NOTIFY messages (which include the PID of the notifying backend
-- process). Note that the PID belongs to a process executing on the
-- database server host, not the local host!
getBackendPID :: Connection -> IO CPid
getBackendPID conn = pure $ fromIntegral $ HPgsql.getBackendPid (hpgConn conn)
