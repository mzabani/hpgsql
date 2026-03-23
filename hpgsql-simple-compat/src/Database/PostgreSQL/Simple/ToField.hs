{-# LANGUAGE CPP #-}
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
    toJSONField,
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
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.Compat (toByteString)
import HPgsql.Encoding (ToPgField (..))

-- | How to render an element when substituting it into a query.
data Action
  = -- | Render without escaping or quoting. Use for non-text types
    -- such as numbers, when you are /certain/ that they will not
    -- introduce formatting vulnerabilities via use of characters such
    -- as spaces or \"@'@\".
    Plain Builder
  | -- | Escape and enclose in quotes before substituting. Use for all
    -- text-like types, and anything else that may contain unsafe
    -- characters when rendered.
    Escape ByteString
  | -- | Escape binary data for use as a @bytea@ literal.  Include surrounding
    -- quotes.  This is used by the 'Binary' newtype wrapper.
    EscapeByteA ByteString
  | -- | Escape before substituting. Use for all sql identifiers like
    -- table, column names, etc. This is used by the 'Identifier' newtype
    -- wrapper.
    EscapeIdentifier ByteString
  | -- | Concatenate a series of rendering actions.
    Many [Action]
  deriving (Typeable)

instance Show Action where
  show (Plain b) = "Plain " ++ show (toByteString b)
  show (Escape b) = "Escape " ++ show b
  show (EscapeByteA b) = "EscapeByteA " ++ show b
  show (EscapeIdentifier b) = "EscapeIdentifier " ++ show b
  show (Many b) = "Many " ++ show b

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
  toField :: a -> Action
  -- ^ Prepare a value for substitution into a query string.

-- | Convert a Haskell value to JSON using 'JSON.toEncoding'.
--
-- This can be used as the default implementation for the 'toField'
-- method for Haskell types that have a JSON representation in
-- PostgreSQL.
toJSONField :: (JSON.ToJSON a) => a -> Action
toJSONField = Escape . SB.concat . LB.toChunks . JSON.encode

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where
    quote = char8 '\''

-- | This instance relies on rendering the Action to text bytes. Since hpgsql
-- sends all parameters using binary format, this will not work correctly for
-- most types at runtime. With OVERLAPPABLE, hpgsql's native ToPgField instances
-- (which produce proper binary encodings) will take precedence for types that
-- have them.
instance {-# OVERLAPPABLE #-} (ToField a) => ToPgField a where
  toTypeOid _ = Nothing
  toPgField a = Just $ actionToLBS (toField a)

actionToLBS :: Action -> LB.ByteString
actionToLBS (Plain b) = LB.fromStrict (toByteString b)
actionToLBS (Escape bs) = LB.fromStrict bs
actionToLBS (EscapeByteA bs) = LB.fromStrict bs
actionToLBS (EscapeIdentifier bs) = LB.fromStrict bs
actionToLBS (Many actions) = LB.concat (map actionToLBS actions)
