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
import Database.PostgreSQL.Simple.HpgsqlUtils
import GHC.Generics (Generic (..), K1 (..), M1 (..), (:*:) (..))
import Hpgsql.Encoding (FieldDecoder, FromPgField (..), FromPgRow (..), singleField)
import Hpgsql.Encoding.RowDecoderMonadic (RowDecoderMonadic, toMonadicRowDecoder)
import Hpgsql.Types (Only (..), (:.) (..))
import Prelude hiding (null)

type RowParser = RowDecoderMonadic

class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (Generic a, ProductTypeDecoder (Rep a)) => RowParser a
  fromRow = genericFromPgRow

fromHpgsqlField :: (FromField a) => FieldDecoder a
fromHpgsqlField = toHpgsqlFieldDecoder fromField

instance (FromField a) => FromRow (Only a) where
  fromRow = Only <$> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b) => FromRow (a, b) where
  fromRow = (,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c) where
  fromRow = (,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d) where
  fromRow = (,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e) where
  fromRow = (,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f) where
  fromRow = (,,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g) => FromRow (a, b, c, d, e, f, g) where
  fromRow = (,,,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h) => FromRow (a, b, c, d, e, f, g, h) where
  fromRow = (,,,,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i) => FromRow (a, b, c, d, e, f, g, h, i) where
  fromRow = (,,,,,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i, FromField j) => FromRow (a, b, c, d, e, f, g, h, i, j) where
  fromRow = (,,,,,,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i, FromField j, FromField k) => FromRow (a, b, c, d, e, f, g, h, i, j, k) where
  fromRow = (,,,,,,,,,,) <$> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField) <*> toMonadicRowDecoder (singleField fromHpgsqlField)

instance (FromRow a, FromRow b) => FromRow (a :. b) where
  fromRow = (:.) <$> fromRow <*> fromRow

field :: (FromPgField a) => RowDecoderMonadic a
field = toMonadicRowDecoder $ singleField fieldDecoder

class ProductTypeDecoder f where
  genRowDecoder :: RowDecoderMonadic (f a)

instance (ProductTypeDecoder a, ProductTypeDecoder b) => ProductTypeDecoder (a :*: b) where
  genRowDecoder = (:*:) <$> genRowDecoder <*> genRowDecoder

instance (ProductTypeDecoder f) => ProductTypeDecoder (M1 a c f) where
  genRowDecoder = M1 <$> genRowDecoder

instance (FromField a) => ProductTypeDecoder (K1 r a) where
  genRowDecoder = fmap K1 $ toMonadicRowDecoder $ singleField $ fromHpgsqlField @a

genericFromPgRow :: forall a. (Generic a, ProductTypeDecoder (Rep a)) => RowParser a
genericFromPgRow = to <$> genRowDecoder @(Rep a)
