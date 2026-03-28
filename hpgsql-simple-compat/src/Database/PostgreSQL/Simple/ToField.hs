{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
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
    inQuotes,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder
  ( Builder,
    char8,
  )
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int16, Int32, Int64)
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar.Compat (Day)
import Data.Time.Compat (UTCTime)
import Data.Time.LocalTime.Compat (CalendarDiffTime, ZonedTime)
import Data.Typeable (Proxy (..), Typeable)
import Database.PostgreSQL.Simple.Types (Binary (..))
import HPgsql.Encoding (ToPgField (..))
import HPgsql.TypeInfo (Oid, TypeInfo)
import HPgsql.Types (Aeson)

-- | How to render an element when substituting it into a query.
data Action
  = QueryArgument (Map Oid TypeInfo -> (Maybe Oid, Maybe LB.ByteString))
  | -- | Escape before substituting. Use for all sql identifiers like
    -- table, column names, etc. This is used by the 'Identifier' newtype
    -- wrapper.
    EscapeIdentifier ByteString
  | -- | Concatenate a series of rendering actions.
    Many [Action]
  deriving (Typeable)

instance Show Action where
  show (QueryArgument _) = "QueryArgument"
  show (EscapeIdentifier b) = "EscapeIdentifier " ++ show b
  show (Many b) = "Many " ++ show b

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
  toField :: a -> Action
  default toField :: (ToPgField a) => a -> Action
  -- TODO: Nothing as the typeOid so postgres has to infer it?
  toField v = QueryArgument $ \tyiCache -> (toTypeOid (proxyOf v) tyiCache, toPgField tyiCache v)

instance ToField Int

instance ToField Int16

instance ToField Int32

instance ToField Int64

instance ToField Integer

instance ToField Oid

instance ToField Scientific

instance ToField Float

instance ToField Double

instance ToField Bool

instance ToField Day

instance ToField CalendarDiffTime

instance ToField UTCTime

instance ToField ZonedTime

instance ToField Char

instance ToField ByteString

instance ToField LB.ByteString

instance ToField Text

instance ToField LT.Text

instance ToField String

instance ToField Aeson.Value

instance (ToField a) => ToField (Maybe a) where
  toField = \case
    Nothing -> QueryArgument $ \_ -> (Nothing, Nothing) -- Postgres will infer the type, which is not the same as hpgsql, but perhaps fine? It's at least similar to what happens when using the text format.
    Just v -> case toField v of
      QueryArgument enc -> QueryArgument enc
      _ -> error "hpgsql-simple-compat does not support (ToField (Maybe a)) instances that aren't simple query arguments"

-- TODO this instance
-- instance (ToField a) => ToField (Vector a) where
--   toField v
--     | Vector.length v == 0 = QueryArgument
--     | otherwise =

instance ToField (Binary ByteString)

instance (Aeson.ToJSON a) => ToField (Aeson a)

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where
    quote = char8 '\''
