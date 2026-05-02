
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
  )
where

import Data.Typeable (Typeable)

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

