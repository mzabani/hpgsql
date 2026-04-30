{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Functor.Contravariant (Contravariant (..))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import GHC.Generics (Generic (..), K1 (..), M1 (..), type (:*:) (..))
import Hpgsql.Encoding (Only (..), ToPgRow (..))
import Hpgsql.Types ((:.) (..))

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
  default toRow :: (Generic a, ProductTypeEncoder (Rep a)) => a -> [Action]
  toRow = toPgParams genericToPgRow
  -- ^ ToField a collection of values.

instance ToRow () where
  toRow () = []

instance (ToField a) => ToRow (Only a) where
  toRow (Only v) = [toField v]

instance (ToField a, ToField b) => ToRow (a, b) where
  toRow (a, b) = [toField a, toField b]

instance (ToField a, ToField b, ToField c) => ToRow (a, b, c) where
  toRow (a, b, c) = [toField a, toField b, toField c]

instance (ToField a, ToField b, ToField c, ToField d) => ToRow (a, b, c, d) where
  toRow (a, b, c, d) = [toField a, toField b, toField c, toField d]

instance (ToField a, ToField b, ToField c, ToField d, ToField e) => ToRow (a, b, c, d, e) where
  toRow (a, b, c, d, e) = [toField a, toField b, toField c, toField d, toField e]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f) => ToRow (a, b, c, d, e, f) where
  toRow (a, b, c, d, e, f) = [toField a, toField b, toField c, toField d, toField e, toField f]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g) => ToRow (a, b, c, d, e, f, g) where
  toRow (a, b, c, d, e, f, g) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h) => ToRow (a, b, c, d, e, f, g, h) where
  toRow (a, b, c, d, e, f, g, h) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h, ToField i) => ToRow (a, b, c, d, e, f, g, h, i) where
  toRow (a, b, c, d, e, f, g, h, i) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h, ToField i, ToField j) => ToRow (a, b, c, d, e, f, g, h, i, j) where
  toRow (a, b, c, d, e, f, g, h, i, j) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h, ToField i, ToField j, ToField k) => ToRow (a, b, c, d, e, f, g, h, i, j, k) where
  toRow (a, b, c, d, e, f, g, h, i, j, k) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k]

instance (ToField a) => ToRow [a] where
  toRow = map toField

instance (ToRow a, ToRow b) => ToRow (a :. b) where
  toRow (a :. b) = toRow a ++ toRow b

-- | This is a simplified version of hpgsql's RowEncoder; it contains just as much as necessary
-- for this module.
newtype RowEncoder a = RowEncoder {toPgParams :: a -> [Action]}

instance Contravariant RowEncoder where
  contramap f rec = RowEncoder (\v -> (toPgParams rec) (f v))

-- | These are from `Divisible`, but we don't currently pull in the extra dependency that has that.
divide :: (a -> (b, c)) -> RowEncoder b -> RowEncoder c -> RowEncoder a
divide d re1 re2 =
  RowEncoder
    { toPgParams = \a -> let (b, c) = d a in (toPgParams re1) b ++ (toPgParams re2) c
    }

genericToPgRow :: forall a. (Generic a, ProductTypeEncoder (Rep a)) => RowEncoder a
genericToPgRow = contramap from genRowEncoder

class ProductTypeEncoder f where
  genRowEncoder :: RowEncoder (f a)

instance (ProductTypeEncoder a, ProductTypeEncoder b) => ProductTypeEncoder (a :*: b) where
  genRowEncoder = divide (\(a :*: b) -> (a, b)) genRowEncoder genRowEncoder

instance (ProductTypeEncoder f) => ProductTypeEncoder (M1 i c f) where
  genRowEncoder = contramap unM1 genRowEncoder

instance (ToField a) => ProductTypeEncoder (K1 r a) where
  genRowEncoder = contramap unK1 (RowEncoder $ (: []) . toField)
