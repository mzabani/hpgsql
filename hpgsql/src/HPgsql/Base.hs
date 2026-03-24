module HPgsql.Base where

import Control.Monad (void)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

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

-- whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
-- whenJust m f = case m of
--   Nothing -> pure ()
--   Just v -> f v

-- whenM :: IO Bool -> IO a -> IO ()
-- whenM cond f = do
--   v <- cond
--   when v $ void f
