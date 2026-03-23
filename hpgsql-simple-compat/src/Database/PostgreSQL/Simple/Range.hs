{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.Range
-- Copyright:   (c) 2014-2015 Leonid Onokhov
--              (c) 2014-2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
module Database.PostgreSQL.Simple.Range
  ( RangeBound (..),
    PGRange (..),
    empty,
    isEmpty,
    isEmptyBy,
    contains,
    containsBy,
    fromFieldRange,
  )
where

import Control.Applicative hiding (empty)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import Data.ByteString.Builder
  ( Builder,
    byteString,
    char8,
  )
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.Compat (toByteString)
import Database.PostgreSQL.Simple.FromField

-- | Represents boundary of a range
data RangeBound a
  = NegInfinity
  | Inclusive !a
  | Exclusive !a
  | PosInfinity
  deriving (Show, Typeable, Eq, Functor)

-- | Generic range type
data PGRange a = PGRange !(RangeBound a) !(RangeBound a)
  deriving (Show, Typeable, Functor)

empty :: PGRange a
empty = PGRange PosInfinity NegInfinity

instance (Ord a) => Eq (PGRange a) where
  x == y = eq x y || (isEmpty x && isEmpty y)
    where
      eq (PGRange a m) (PGRange b n) = a == b && m == n

isEmptyBy :: (a -> a -> Ordering) -> PGRange a -> Bool
isEmptyBy cmp v =
  case v of
    (PGRange PosInfinity _) -> True
    (PGRange _ NegInfinity) -> True
    (PGRange NegInfinity _) -> False
    (PGRange _ PosInfinity) -> False
    (PGRange (Inclusive x) (Inclusive y)) -> cmp x y == GT
    (PGRange (Inclusive x) (Exclusive y)) -> cmp x y /= LT
    (PGRange (Exclusive x) (Inclusive y)) -> cmp x y /= LT
    (PGRange (Exclusive x) (Exclusive y)) -> cmp x y /= LT

-- | Is a range empty?   If this returns 'True',  then the 'contains'
--   predicate will always return 'False'.   However,  if this returns
--   'False', it is not necessarily true that there exists a point for
--   which 'contains' returns 'True'.
--   Consider @'PGRange' ('Excludes' 2) ('Excludes' 3) :: PGRange Int@,
--   for example.
isEmpty :: (Ord a) => PGRange a -> Bool
isEmpty = isEmptyBy compare

-- | Does a range contain a given point?   Note that in some cases, this may
-- not correspond exactly with a server-side computation.   Consider @UTCTime@
-- for example, which has a resolution of a picosecond,  whereas postgresql's
-- @timestamptz@ types have a resolution of a microsecond.  Putting such
-- Haskell values into the database will result in them being rounded, which
-- can change the value of the containment predicate.
contains :: (Ord a) => PGRange a -> (a -> Bool)
contains = containsBy compare

containsBy :: (a -> a -> Ordering) -> PGRange a -> (a -> Bool)
containsBy cmp rng x =
  case rng of
    PGRange _lb NegInfinity -> False
    PGRange lb ub -> checkLB lb x && checkUB ub x
  where
    checkLB lb y =
      case lb of
        NegInfinity -> True
        PosInfinity -> False
        Inclusive a -> cmp a y /= GT
        Exclusive a -> cmp a y == LT

    checkUB ub y =
      case ub of
        NegInfinity -> False
        PosInfinity -> True
        Inclusive z -> cmp y z /= GT
        Exclusive z -> cmp y z == LT

lowerBound :: Parser (a -> RangeBound a)
lowerBound = (A.char '(' *> pure Exclusive) <|> (A.char '[' *> pure Inclusive)
{-# INLINE lowerBound #-}

upperBound :: Parser (a -> RangeBound a)
upperBound = (A.char ')' *> pure Exclusive) <|> (A.char ']' *> pure Inclusive)
{-# INLINE upperBound #-}

-- | Generic range parser
pgrange :: Parser (RangeBound B.ByteString, RangeBound B.ByteString)
pgrange = do
  lb <- lowerBound
  v1 <- (A.char ',' *> "") <|> (rangeElem (== ',') <* A.char ',')
  v2 <- rangeElem $ \c -> c == ')' || c == ']'
  ub <- upperBound
  A.endOfInput
  let low = if B.null v1 then NegInfinity else lb v1
      up = if B.null v2 then PosInfinity else ub v2
  return (low, up)

rangeElem :: (Char -> Bool) -> Parser B.ByteString
rangeElem end =
  (A.char '"' *> doubleQuoted)
    <|> A.takeTill end
{-# INLINE rangeElem #-}

-- | Simple double quoted value parser
doubleQuoted :: Parser B.ByteString
doubleQuoted = toByteString <$> go mempty
  where
    go acc = do
      h <- byteString <$> A.takeTill (\c -> c == '\\' || c == '"')
      let rest = do
            start <- A.anyChar
            case start of
              '\\' -> do
                c <- A.anyChar
                go (acc <> h <> char8 c)
              '"' ->
                (A.char '"' *> go (acc <> h <> char8 '"'))
                  <|> pure (acc <> h)
              _ -> error "impossible in doubleQuoted"
      rest

rangeToBuilder :: (Ord a) => (a -> Builder) -> PGRange a -> Builder
rangeToBuilder = rangeToBuilderBy compare

-- | Generic range to builder for plain values
rangeToBuilderBy :: (a -> a -> Ordering) -> (a -> Builder) -> PGRange a -> Builder
rangeToBuilderBy cmp f x =
  if isEmptyBy cmp x
    then byteString "'empty'"
    else
      let (PGRange a b) = x
       in buildLB a <> buildUB b
  where
    buildLB NegInfinity = byteString "'[,"
    buildLB (Inclusive v) = byteString "'[\"" <> f v <> byteString "\","
    buildLB (Exclusive v) = byteString "'(\"" <> f v <> byteString "\","
    buildLB PosInfinity = error "impossible in rangeToBuilder"

    buildUB NegInfinity = error "impossible in rangeToBuilder"
    buildUB (Inclusive v) = char8 '"' <> f v <> byteString "\"]'"
    buildUB (Exclusive v) = char8 '"' <> f v <> byteString "\")'"
    buildUB PosInfinity = byteString "]'"

{-# INLINE rangeToBuilder #-}

fromFieldRange :: (Typeable a) => FieldParser a -> FieldParser (PGRange a)
fromFieldRange fromField' f mdat = do
  info <- typeInfo f
  case info of
    Range {} ->
      let f' = f {typeOid = typoid (rngsubtype info)}
       in case mdat of
            Nothing -> returnError UnexpectedNull f ""
            Just "empty" -> pure $ empty
            Just bs ->
              let parseIt NegInfinity = pure NegInfinity
                  parseIt (Inclusive v) = Inclusive <$> fromField' f' (Just v)
                  parseIt (Exclusive v) = Exclusive <$> fromField' f' (Just v)
                  parseIt PosInfinity = pure PosInfinity
               in case parseOnly pgrange bs of
                    Left e -> returnError ConversionFailed f e
                    Right (lb, ub) -> PGRange <$> parseIt lb <*> parseIt ub
    _ -> returnError Incompatible f ""

