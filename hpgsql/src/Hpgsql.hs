module Hpgsql
  ( -- * Connection
    connect,
    connectOpts,
    defaultConnectOpts,
    withConnection,
    withConnectionOpts,
    closeGracefully,
    closeForcefully,

    -- * Query
    query,
    queryWith,
    queryWithM,
    queryStreaming,
    queryWithStreaming,
    queryWithStreamingM,

    -- * Execute
    execute,
    execute_,
    executeMany,
    executeMany_,

    -- * Type info
    refreshTypeInfoCache,
    resetTypeInfoCache,
    getParameterStatus,
    getBackendPid,

    -- * Type re-exports
    HPgConnection, -- Do not export constructor
    Query, -- Do not export constructor
    ConnString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    RowParser (..),
    TransactionStatus (..),
    Only (..),
    FromPgRow (..),
    FromPgField (..),
  )
where

import Hpgsql.Encoding (FromPgField (..), FromPgRow (..), Only (..), RowParser (..))
import Hpgsql.Internal
import Hpgsql.InternalTypes (ConnString (..), ConnectOpts (..), ErrorDetail (..), HPgConnection, IrrecoverableHpgsqlError (..), PostgresError (..), TransactionStatus (..))
import Hpgsql.Query (Query)
