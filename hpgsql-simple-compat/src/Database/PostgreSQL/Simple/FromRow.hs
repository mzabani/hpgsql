{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

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
    FromPgRow (..),
    field,
  )
where

import Database.PostgreSQL.Simple.FromField (FromField (..))
import HPgsql.Encoding (FromPgField (..), FromPgRow (..), Only (..), RowParser, singleColRowParser, (:.) (..))
import Prelude hiding (null)

class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (FromPgRow a) => RowParser a
  fromRow = rowParser

instance (FromField a) => FromRow (Only a) where
  fromRow = Only <$> singleColRowParser fromField

instance (FromField a, FromField b) => FromRow (a, b) where
  fromRow = (,) <$> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c) where
  fromRow = (,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d) where
  fromRow = (,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e) where
  fromRow = (,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f) where
  fromRow = (,,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g) => FromRow (a, b, c, d, e, f, g) where
  fromRow = (,,,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h) => FromRow (a, b, c, d, e, f, g, h) where
  fromRow = (,,,,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i) => FromRow (a, b, c, d, e, f, g, h, i) where
  fromRow = (,,,,,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i, FromField j) => FromRow (a, b, c, d, e, f, g, h, i, j) where
  fromRow = (,,,,,,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i, FromField j, FromField k) => FromRow (a, b, c, d, e, f, g, h, i, j, k) where
  fromRow = (,,,,,,,,,,) <$> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField <*> singleColRowParser fromField

instance (FromRow a, FromRow b) => FromRow (a :. b) where
  fromRow = (:.) <$> fromRow <*> fromRow

field :: (FromPgField a) => RowParser a
field = singleColRowParser fieldParser
