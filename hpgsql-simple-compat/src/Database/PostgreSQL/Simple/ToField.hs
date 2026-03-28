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
    ToPgField (..),
    inQuotes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
  ( Builder,
    char8,
  )
import qualified Data.ByteString.Lazy as LB
import Data.Map.Strict (Map)
import Data.Typeable (Proxy (..), Typeable)
import HPgsql.Encoding (ToPgField (..))
import HPgsql.TypeInfo (Oid, TypeInfo)

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

instance (ToPgField a) => ToField a

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where
    quote = char8 '\''
