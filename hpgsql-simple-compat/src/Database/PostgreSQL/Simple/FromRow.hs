{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.FromRow
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- The 'FromRow' typeclass, for converting a row of results
-- returned by a SQL query into a more useful Haskell representation.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.  The instances for 'Maybe' types return 'Nothing' if all
-- the columns that would have been otherwise consumed are null,  otherwise
-- it attempts a regular conversion.
module Database.PostgreSQL.Simple.FromRow
  ( FromRow (..),
    RowParser,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, replicateM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Compat
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.TypeInfo
import Database.PostgreSQL.Simple.Types (Null, Only (..), (:.) (..))
import GHC.Generics
import Prelude hiding (null)

-- | A collection type that can be converted from a sequence of fields.
-- Instances are provided for tuples up to 10 elements and lists of any length.
--
-- Note that instances can be defined outside of hpgsql-simple-compat,  which is
-- often useful.   For example, here's an instance for a user-defined pair:
--
-- @
-- data User = User { name :: String, fileQuota :: Int }
--
-- instance 'FromRow' User where
--     fromRow = User \<$\> 'field' \<*\> 'field'
-- @
--
-- The number of calls to 'field' must match the number of fields returned
-- in a single row of the query result.  Otherwise,  a 'ConversionFailed'
-- exception will be thrown.
--
-- You can also derive 'FromRow' for your data type using GHC generics, like
-- this:
--
-- @
-- \{-# LANGUAGE DeriveAnyClass \#-}
-- \{-# LANGUAGE DeriveGeneric  \#-}
--
-- import "GHC.Generics" ('GHC.Generics.Generic')
-- import "Database.PostgreSQL.Simple" ('FromRow')
--
-- data User = User { name :: String, fileQuota :: Int }
--   deriving ('GHC.Generics.Generic', 'FromRow')
-- @
--
-- Note that this only works for product types (e.g. records) and does not
-- support sum types or recursive types.
--
-- Note that 'field' evaluates its result to WHNF, so the caveats listed in
-- mysql-simple and very early versions of hpgsql-simple-compat no longer apply.
-- Instead, look at the caveats associated with user-defined implementations
-- of 'fromField'.
class FromRow a where
  fromRow :: RowParser a

