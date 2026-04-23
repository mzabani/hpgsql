module Database.PostgreSQL.Query.SqlBuilder.Types
  ( -- * Sql builder result
    SqlBuilderResult (..),
    builderResultPure,

    -- * Field masking in logs
    FieldOption (..),
    LogMasker,
    defaultLogMasker,
  )
where

import Data.ByteString (ByteString)
import qualified Hpgsql.InternalTypes as HQ

-- | Result of SqlBuilder. In hpgsql-simple-compat the query string is an
-- hpgsql 'HQ.Query' and logging is a no-op.
data SqlBuilderResult = SqlBuilderResult
  { sbQueryString :: HQ.Query,
    sbLogString :: ()
  }

instance Semigroup SqlBuilderResult where
  (SqlBuilderResult a ()) <> (SqlBuilderResult a' ()) =
    SqlBuilderResult (a <> a') ()

instance Monoid SqlBuilderResult where
  mempty = SqlBuilderResult "" ()

builderResultPure :: HQ.Query -> SqlBuilderResult
builderResultPure q = SqlBuilderResult q ()

-- | Option for field instructing 'LogMasker' what to do with field when logging
data FieldOption
  = -- | Do nothing. Field should be pasted as is
    FieldDefault
  | -- | Mask field in logs with placeholder.
    FieldMasked
  deriving (Eq, Ord, Show)

-- | Function modifying query parameter value before pasting it to log.
-- In hpgsql-simple-compat this is a no-op since logging is not supported,
-- but the type is preserved for some API compatibility.
type LogMasker = FieldOption -> ByteString -> ByteString

-- | Simply replaces masked fields with placeholder.
defaultLogMasker :: LogMasker
defaultLogMasker FieldDefault bb = bb
defaultLogMasker FieldMasked _ = "<MASKED BY POSTGRESQL-QUERY>"
