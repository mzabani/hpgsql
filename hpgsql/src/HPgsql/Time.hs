module HPgsql.Time
  ( Unbounded (..),
  )
where

-- | Use this type to represent -infinity and +infinity.
data Unbounded a
  = NegInfinity
  | Finite !a
  | PosInfinity
  deriving (Eq, Ord, Functor)

instance (Show a) => Show (Unbounded a) where
  show =
    \case
      NegInfinity -> "-infinity"
      Finite time -> show time
      PosInfinity -> "infinity"
