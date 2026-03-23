{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
    inQuotes,
  )
where

import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder
  ( Builder,
    char8,
  )
import qualified Data.ByteString.Lazy as LB
import Data.Typeable (Proxy (..), Typeable)
import Database.PostgreSQL.Simple.Compat (toByteString)
import HPgsql.Encoding (ToPgField (..))
import qualified HPgsql.Encoding as HPgsql
import HPgsql.TypeInfo (Oid)

-- | How to render an element when substituting it into a query.
data Action
  = QueryArgument (Maybe Oid, Maybe LB.ByteString)
  | -- | Escape before substituting. Use for all sql identifiers like
    -- table, column names, etc. This is used by the 'Identifier' newtype
    -- wrapper.
    EscapeIdentifier ByteString
  | -- | Concatenate a series of rendering actions.
    Many [Action]
  deriving (Typeable)

instance Show Action where
  show (QueryArgument (oid, _binRep)) = "QueryArgument " ++ show (oid)
  show (EscapeIdentifier b) = "EscapeIdentifier " ++ show b
  show (Many b) = "Many " ++ show b

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
  toField :: a -> Action
  default toField :: (HPgsql.ToPgField a) => a -> Action
  toField v = QueryArgument (HPgsql.toTypeOid (proxyOf v), HPgsql.toPgField v)

instance (HPgsql.ToPgField a) => ToField a

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where
    quote = char8 '\''
