module Hpgsql.Pool
  ( PoolCleanup (..),
    beforeReturningToPool,
  )
where

import Hpgsql.Internal (beforeReturningToPool)
import Hpgsql.InternalTypes (PoolCleanup (..))
