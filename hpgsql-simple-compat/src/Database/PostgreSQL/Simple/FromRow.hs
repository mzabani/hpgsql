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

import HPgsql.Encoding (FromPgField (..), FromPgRow (..), RowParser, singleColRowParser)
import Prelude hiding (null)

class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (FromPgRow a) => RowParser a
  fromRow = rowParser

instance (FromPgRow a) => FromRow a

field :: (FromPgField a) => RowParser a
field = singleColRowParser fieldParser
