-- | Module with newtypes suitable to usage with @DerivingVia@ or standalone.
--
-- The newtypes are named after packages they wrap.
module Database.PostgreSQL.Simple.Newtypes
  ( Aeson (..),
  )
where

import Hpgsql.Types (Aeson (..))
