{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.ToField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- The 'ToField' typeclass, for rendering a parameter to a SQL query.
module Database.PostgreSQL.Simple.ToField
  ( Action (..),
    ToField (..),
    ToPgField (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar.Compat (Day)
import Data.Time.Compat (CalendarDiffTime, NominalDiffTime, UTCTime, ZonedTime)
import Data.Time.LocalTime.Compat (LocalTime)
import Data.Typeable (Proxy (..), Typeable)
import Data.CaseInsensitive (CI)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Hpgsql.Builder (BinaryField (..))
import Hpgsql.Encoding (EncodingContext, ToPgField (..))
import Hpgsql.Time (Unbounded (..))
import Hpgsql.TypeInfo (Oid)
import Hpgsql.Types (Aeson, PGArray)

-- | How to render an element when substituting it into a query.
data Action
  = QueryArgument (EncodingContext -> (Maybe Oid, BinaryField))
  | -- | Escape before substituting. Use for all sql identifiers like
    -- table, column names, etc. This is used by the 'Identifier' newtype
    -- wrapper.
    EscapeIdentifier ByteString
  | -- | Concatenate a series of rendering actions.
    Many [Action]
  | -- | Just a static SQL fragment to render
    Plain LB.ByteString
  deriving (Typeable)

instance Show Action where
  show (QueryArgument _) = "QueryArgument"
  show (EscapeIdentifier b) = "EscapeIdentifier " ++ show b
  show (Many b) = "Many " ++ show b
  show (Plain b) = "Plain " ++ show b

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
  toField :: a -> Action
  default toField :: (ToPgField a) => a -> Action
  toField v = QueryArgument $ \encCtx -> (toTypeOid (proxyOf v) encCtx, toPgField encCtx v)

instance ToField Action where
  toField v = v

instance ToField Int

instance ToField Int16

instance ToField Int32

instance ToField Int64

instance ToField Integer

instance ToField Oid

instance ToField Scientific

instance ToField Rational

instance ToField Float

instance ToField Double

instance ToField Bool

instance ToField Day

instance ToField CalendarDiffTime

instance ToField NominalDiffTime

instance ToField UTCTime

instance ToField ZonedTime

instance ToField LocalTime

instance ToField Char

instance ToField ByteString

instance ToField LB.ByteString

instance ToField Text

instance ToField LT.Text

instance ToField String

instance ToField Aeson.Value

instance ToField UUID

instance ToField (CI Text)

instance ToField (CI LT.Text)

instance ToField (CI String)

instance ToField (Unbounded Day)

instance ToField (Unbounded UTCTime)

instance ToField (Unbounded ZonedTime)

class HasFieldType a where
  typeOid :: Proxy a -> EncodingContext -> Maybe Oid

instance (ToPgField a) => HasFieldType a where
  typeOid = toTypeOid

instance forall a. (ToField a, HasFieldType a) => ToField (Maybe a) where
  toField = \case
    Nothing -> QueryArgument $ \encCtx -> (typeOid (Proxy @a) encCtx, SqlNull)
    Just v -> case toField v of
      QueryArgument enc -> QueryArgument enc
      _ -> error "hpgsql-simple-compat does not support (ToField (Maybe a)) instances that aren't simple query arguments"

instance (ToPgField a) => ToField (Vector a)

instance (ToPgField a) => ToField (PGArray a)

instance (Aeson.ToJSON a) => ToField (Aeson a)
