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
import qualified Data.List as List
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar.Compat (Day)
import Data.Time.Compat (CalendarDiffTime, NominalDiffTime, UTCTime, ZonedTime)
import Data.Typeable (Proxy (..), Typeable)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.Types (Binary (..), Identifier (..), In (..))
import Debug.Trace
import HPgsql.Encoding (EncodingContext, ToPgField (..))
import HPgsql.Time (Unbounded (..))
import HPgsql.TypeInfo (Oid)
import HPgsql.Types (Aeson, PGArray)

-- | How to render an element when substituting it into a query.
data Action
  = QueryArgument (EncodingContext -> (Maybe Oid, Maybe LB.ByteString))
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
  -- TODO: Nothing as the typeOid so postgres has to infer it?
  toField v = QueryArgument $ \encCtx -> (toTypeOid (proxyOf v) encCtx, toPgField encCtx v)

instance ToField Action where
  toField v = v

instance ToField Identifier where
  toField (Identifier ident) = EscapeIdentifier $ encodeUtf8 ident

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

instance ToField NominalDiffTime

instance ToField UTCTime

instance ToField ZonedTime

instance ToField Char

instance ToField ByteString

instance ToField LB.ByteString

instance ToField Text

instance ToField LT.Text

instance ToField String

instance ToField Aeson.Value

instance (ToField a) => ToField (In [a]) where
  toField (In []) = Plain "(NULL)"
  toField (In xs) = Many $ Plain "(" : List.intersperse (Plain ",") (map toField xs) ++ [Plain ")"]

instance ToField (Unbounded Day)

instance ToField (Unbounded UTCTime)

instance ToField (Unbounded ZonedTime)

instance (ToField a) => ToField (Maybe a) where
  toField = \case
    Nothing -> QueryArgument $ \_ -> (Nothing, Nothing) -- Postgres will infer the type, which is not the same as hpgsql, but perhaps fine? It's at least similar to what happens when using the text format.
    Just v -> case toField v of
      QueryArgument enc -> QueryArgument enc
      _ -> error "hpgsql-simple-compat does not support (ToField (Maybe a)) instances that aren't simple query arguments"

instance (ToPgField a) => ToField (Vector a)

instance (ToPgField a) => ToField (PGArray a)

instance ToField (Binary ByteString)

instance (Aeson.ToJSON a) => ToField (Aeson a)

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where
    quote = char8 '\''
