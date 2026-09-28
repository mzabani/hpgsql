module Hpgsql.Notification
  ( getNotification,
    getNotificationNonBlocking,
    NotificationResponse (..),
  )
where

import Hpgsql.Internal (getNotification, getNotificationNonBlocking)
import Hpgsql.Internal.InternalTypes (NotificationResponse (..))
