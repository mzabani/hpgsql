module Database.PostgreSQL.Simple.FromField where

import Database.PostgreSQL.Simple.Types

class FromField a

instance FromField Oid
