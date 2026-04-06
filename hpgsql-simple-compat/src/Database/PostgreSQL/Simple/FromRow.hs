{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    RowParser,
    field,
  )
where

import Database.PostgreSQL.Simple.FromField (FromField (..))
import GHC.Generics (Generic (..))
import HPgsql.Encoding (FromPgField (..), FromPgRow (..), Only (..), ProductTypeDecoder (..), RowParserMonadic, singleColRowParser, toMonadicRowParser, (:.) (..))
import Prelude hiding (null)

type RowParser = RowParserMonadic

class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (Generic a, ProductTypeDecoder (Rep a)) => RowParser a
  fromRow = genericFromPgRow

instance (FromField a) => FromRow (Only a) where
  fromRow = Only <$> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b) => FromRow (a, b) where
  fromRow = (,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c) where
  fromRow = (,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d) where
  fromRow = (,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e) where
  fromRow = (,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f) where
  fromRow = (,,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g) => FromRow (a, b, c, d, e, f, g) where
  fromRow = (,,,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h) => FromRow (a, b, c, d, e, f, g, h) where
  fromRow = (,,,,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i) => FromRow (a, b, c, d, e, f, g, h, i) where
  fromRow = (,,,,,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i, FromField j) => FromRow (a, b, c, d, e, f, g, h, i, j) where
  fromRow = (,,,,,,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i, FromField j, FromField k) => FromRow (a, b, c, d, e, f, g, h, i, j, k) where
  fromRow = (,,,,,,,,,,) <$> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField) <*> toMonadicRowParser (singleColRowParser fromField)

instance (FromRow a, FromRow b) => FromRow (a :. b) where
  fromRow = (:.) <$> fromRow <*> fromRow

field :: (FromPgField a) => RowParser a
field = toMonadicRowParser $ singleColRowParser fieldParser

genericFromPgRow :: forall a. (Generic a, ProductTypeDecoder (Rep a)) => RowParser a
genericFromPgRow = toMonadicRowParser $ to <$> genRowDecoder @(Rep a)
