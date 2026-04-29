module Database.PostgreSQL.Query
  ( -- * Common usage modules

    -- module Database.PostgreSQL.Query.Entity
    module Database.PostgreSQL.Query.Functions,
    module Database.PostgreSQL.Query.SqlBuilder,
    module Database.PostgreSQL.Query.TH,
    module Database.PostgreSQL.Query.Types,

    -- * Some re-exports from postgresql-simple
    Connection,
    connect,
    defaultConnectInfo,
    connectPostgreSQL,
    ConnectInfo (..),
    ToField (..),
    ToRow (..),
    FromField (..),
    FromRow (..),
    Query (..),
    Only (..),
    In (..),
    Oid (..),
    Values (..),
    (:.) (..),
    PGArray (..),
    HStoreList (..),
    HStoreMap (..),
    ToHStore (..),
    HStoreBuilder,
    hstore,
    parseHStoreList,
    ToHStoreText (..),
    HStoreText,
  )
where

-- to prevent conflicts

-- import Database.PostgreSQL.Query.Entity
import Database.PostgreSQL.Query.Functions
import Database.PostgreSQL.Query.SqlBuilder
import Database.PostgreSQL.Query.TH
import Database.PostgreSQL.Query.Types
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    FromRow,
    ToRow,
    connect,
    connectPostgreSQL,
    defaultConnectInfo,
  )
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.HStore hiding
  ( toBuilder,
    toLazyByteString,
  )
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types
