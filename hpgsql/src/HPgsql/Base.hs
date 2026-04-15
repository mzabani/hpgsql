module HPgsql.Base where

import Control.Monad (void)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)

data StrictTuple a b = StrictTuple !a !b

instance (Semigroup a, Semigroup b) => Semigroup (StrictTuple a b) where
  StrictTuple a1 b1 <> StrictTuple a2 b2 = StrictTuple (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (StrictTuple a b) where
  mempty = StrictTuple mempty mempty

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs

lastAndInit :: [a] -> ([a], Maybe a)
lastAndInit xs = case NE.nonEmpty xs of
  Nothing -> ([], Nothing)
  Just nxs -> second Just $ lastAndInitNE nxs

lastAndInitNE :: NonEmpty a -> ([a], a)
lastAndInitNE (x :| xs) =
  case NE.nonEmpty xs of
    Nothing -> ([], x)
    Just neXs ->
      let (others, l) = lastAndInitNE neXs
       in (x : others, l)

whenNonEmpty :: [a] -> (NonEmpty a -> IO b) -> IO ()
whenNonEmpty [] _ = pure ()
whenNonEmpty (x : xs) f = void $ f (x :| xs)

ifM :: IO Bool -> IO a -> IO a -> IO a
ifM cond fTrue fFalse = do
  v <- cond
  if v then fTrue else fFalse

-- | Takes the minimum of the Just values only, or the default if there are none.
minimumOnOrDef :: (Ord b) => b -> [a] -> (a -> Maybe b) -> b
minimumOnOrDef def xs f =
  case mapMaybe f xs of
    [] -> def
    ls -> minimum ls

-- | Takes the maximum of the Just values only, or the default if there are none.
maximumOnOrDef :: (Ord b) => b -> [a] -> (a -> Maybe b) -> b
maximumOnOrDef def xs f =
  case mapMaybe f xs of
    [] -> def
    ls -> maximum ls

-- whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
-- whenJust m f = case m of
--   Nothing -> pure ()
--   Just v -> f v

-- whenM :: IO Bool -> IO a -> IO ()
-- whenM cond f = do
--   v <- cond
--   when v $ void f
