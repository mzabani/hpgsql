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
    inQuotes,
  )
where

import Control.Applicative (Const (Const))
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder
  ( Builder,
    byteString,
    char8,
    int8Dec,
    word16Dec,
    word32Dec,
    word64Dec,
    word8Dec,
    wordDec,
  )
import qualified Data.ByteString.Lazy as LB
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8)
import Data.List (intersperse)
import qualified Data.Text.Encoding as ST
import Data.Time.Compat (LocalTime, NominalDiffTime, TimeOfDay)
import Data.Typeable (Typeable)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Compat (toByteString)
import Database.PostgreSQL.Simple.Time
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Foreign.C.Types (CUInt (..))
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

instance ToField Action where
  toField a = a
  {-# INLINE toField #-}

instance (ToField a) => ToField (Const a b) where
  toField (Const a) = toField a
  {-# INLINE toField #-}

instance (ToField a) => ToField (Identity a) where
  toField (Identity a) = toField a
  {-# INLINE toField #-}

instance (ToField a) => ToField (In [a]) where
  toField (In []) = Plain $ byteString "(null)"
  toField (In xs) =
    Many $
      Plain (char8 '(')
        : (intersperse (Plain (char8 ',')) . map toField $ xs)
        ++ [Plain (char8 ')')]

renderNull :: Action
renderNull = Plain (byteString "null")

instance ToField Null where
  toField _ = renderNull
  {-# INLINE toField #-}

instance ToField Default where
  toField _ = Plain (byteString "default")
  {-# INLINE toField #-}

instance ToField Int8 where
  toField = Plain . int8Dec
  {-# INLINE toField #-}

instance ToField Word8 where
  toField = Plain . word8Dec
  {-# INLINE toField #-}

instance ToField Word16 where
  toField = Plain . word16Dec
  {-# INLINE toField #-}

instance ToField Word32 where
  toField = Plain . word32Dec
  {-# INLINE toField #-}

instance ToField Word where
  toField = Plain . wordDec
  {-# INLINE toField #-}

instance ToField Word64 where
  toField = Plain . word64Dec
  {-# INLINE toField #-}

instance ToField PQ.Oid where
  toField = Plain . \(PQ.Oid (CUInt x)) -> word32Dec x
  {-# INLINE toField #-}

instance ToField (Binary SB.ByteString) where
  toField (Binary bs) = EscapeByteA bs
  {-# INLINE toField #-}

instance ToField (Binary LB.ByteString) where
  toField (Binary bs) = (EscapeByteA . SB.concat . LB.toChunks) bs
  {-# INLINE toField #-}

instance ToField Identifier where
  toField (Identifier bs) = EscapeIdentifier (ST.encodeUtf8 bs)
  {-# INLINE toField #-}

instance ToField QualifiedIdentifier where
  toField (QualifiedIdentifier (Just s) t) =
    Many
      [ EscapeIdentifier (ST.encodeUtf8 s),
        Plain (char8 '.'),
        EscapeIdentifier (ST.encodeUtf8 t)
      ]
  toField (QualifiedIdentifier Nothing t) =
    EscapeIdentifier (ST.encodeUtf8 t)
  {-# INLINE toField #-}

instance ToField LocalTime where
  toField = Plain . inQuotes . localTimeToBuilder
  {-# INLINE toField #-}

instance ToField TimeOfDay where
  toField = Plain . inQuotes . timeOfDayToBuilder
  {-# INLINE toField #-}

instance ToField UTCTimestamp where
  toField = Plain . inQuotes . utcTimestampToBuilder
  {-# INLINE toField #-}

instance ToField ZonedTimestamp where
  toField = Plain . inQuotes . zonedTimestampToBuilder
  {-# INLINE toField #-}

instance ToField LocalTimestamp where
  toField = Plain . inQuotes . localTimestampToBuilder
  {-# INLINE toField #-}

instance ToField Date where
  toField = Plain . inQuotes . dateToBuilder
  {-# INLINE toField #-}

instance ToField NominalDiffTime where
  toField = Plain . inQuotes . nominalDiffTimeToBuilder
  {-# INLINE toField #-}

instance (ToField a) => ToField (PGArray a) where
  toField pgArray =
    case fromPGArray pgArray of
      [] -> Plain (byteString "'{}'")
      xs ->
        Many $
          Plain (byteString "ARRAY[")
            : (intersperse (Plain (char8 ',')) . map toField $ xs)
            ++ [Plain (char8 ']')]

-- Because the ARRAY[...] input syntax is being used, it is possible
-- that the use of type-specific separator characters is unnecessary.

instance (ToField a) => ToField (Vector a) where
  toField = toField . PGArray . V.toList

instance ToField UUID where
  toField = Plain . inQuotes . byteString . UUID.toASCIIBytes

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where
    quote = char8 '\''

interleaveFoldr :: (a -> [b] -> [b]) -> b -> [b] -> [a] -> [b]
interleaveFoldr f b bs' as = foldr (\a bs -> b : f a bs) bs' as
{-# INLINE interleaveFoldr #-}

instance (ToRow a) => ToField (Values a) where
  toField (Values types rows) =
    case rows of
      [] -> case types of
        [] -> error norows
        (_ : _) ->
          values $
            typedRow
              (repeat (lit "null"))
              types
              [lit " LIMIT 0)"]
      (_ : _) -> case types of
        [] -> values $ untypedRows rows [litC ')']
        (_ : _) -> values $ typedRows rows types [litC ')']
    where
      funcname = "Database.PostgreSQL.Simple.toField :: Values a -> Action"
      norows = funcname ++ "  either values or types must be non-empty"
      emptyrow = funcname ++ "  each row must contain at least one column"
      lit = Plain . byteString
      litC = Plain . char8
      values x = Many (lit "(VALUES " : x)

      typedField :: (Action, QualifiedIdentifier) -> [Action] -> [Action]
      typedField (val, typ) rest = val : lit "::" : toField typ : rest

      typedRow :: [Action] -> [QualifiedIdentifier] -> [Action] -> [Action]
      typedRow (val : vals) (typ : typs) rest =
        litC '('
          : typedField
            (val, typ)
            ( interleaveFoldr
                typedField
                (litC ',')
                (litC ')' : rest)
                (zip vals typs)
            )
      typedRow _ _ _ = error emptyrow

      untypedRow :: [Action] -> [Action] -> [Action]
      untypedRow (val : vals) rest =
        litC '('
          : val
          : interleaveFoldr
            (:)
            (litC ',')
            (litC ')' : rest)
            vals
      untypedRow _ _ = error emptyrow

      typedRows :: (ToRow a) => [a] -> [QualifiedIdentifier] -> [Action] -> [Action]
      typedRows [] _ _ = error funcname
      typedRows (val : vals) typs rest =
        typedRow (toRow val) typs (multiRows vals rest)

      untypedRows :: (ToRow a) => [a] -> [Action] -> [Action]
      untypedRows [] _ = error funcname
      untypedRows (val : vals) rest =
        untypedRow (toRow val) (multiRows vals rest)

      multiRows :: (ToRow a) => [a] -> [Action] -> [Action]
      multiRows vals rest =
        interleaveFoldr
          (untypedRow . toRow)
          (litC ',')
          rest
          vals

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
