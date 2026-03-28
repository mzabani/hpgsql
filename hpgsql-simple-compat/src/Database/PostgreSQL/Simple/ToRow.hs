{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.ToRow
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- The 'ToRow' typeclass, for rendering a collection of
-- parameters to a SQL query.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.
module Database.PostgreSQL.Simple.ToRow
  ( ToRow (..),
    ToPgRow (..),
  )
where

import Database.PostgreSQL.Simple.ToField (Action (..))
import HPgsql.Encoding (Only, ToPgField, ToPgRow (..), (:.) (..))

-- | A collection type that can be turned into a list of rendering
-- 'Action's.
--
-- Instances should use the 'toField' method of the 'ToField' class
-- to perform conversion of each element of the collection.
--
-- You can derive 'ToRow' for your data type using GHC generics, like this:
--
-- @
-- \{-# LANGUAGE DeriveAnyClass \#-}
-- \{-# LANGUAGE DeriveGeneric  \#-}
--
-- import "GHC.Generics" ('GHC.Generics.Generic')
-- import "Database.PostgreSQL.Simple" ('ToRow')
--
-- data User = User { name :: String, fileQuota :: Int }
--   deriving ('GHC.Generics.Generic', 'ToRow')
-- @
--
-- Note that this only works for product types (e.g. records) and does not
-- support sum types or recursive types.
class ToRow a where
  toRow :: a -> [Action]
  default toRow :: (ToPgRow a) => a -> [Action]
  toRow = map QueryArgument . toPgParams
  -- ^ ToField a collection of values.

instance ToRow ()

instance (ToPgField a) => ToRow (Only a)

instance (ToPgField a, ToPgField b) => ToRow (a, b)

instance (ToPgField a, ToPgField b, ToPgField c) => ToRow (a, b, c)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d) => ToRow (a, b, c, d)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e) => ToRow (a, b, c, d, e)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f) => ToRow (a, b, c, d, e, f)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g) => ToRow (a, b, c, d, e, f, g)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h) => ToRow (a, b, c, d, e, f, g, h)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i) => ToRow (a, b, c, d, e, f, g, h, i)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i, ToPgField j) => ToRow (a, b, c, d, e, f, g, h, i, j)

instance (ToPgField a, ToPgField b, ToPgField c, ToPgField d, ToPgField e, ToPgField f, ToPgField g, ToPgField h, ToPgField i, ToPgField j, ToPgField k) => ToRow (a, b, c, d, e, f, g, h, i, j, k)

instance (ToPgField a) => ToRow [a]

instance (ToRow a, ToRow b) => ToRow (a :. b) where
  toRow (a :. b) = toRow a ++ toRow b
