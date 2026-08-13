{-# OPTIONS_GHC -ddump-simpl -dno-typeable-binds -dsuppress-coercions -dsuppress-module-prefixes -dsuppress-type-applications -ddump-to-file #-}

-- |
-- This is not a real test module. It's just a type deriving `FromPgRow`
-- so we can look at GHC Core output. It's as small as we can make it to
-- facilitate reading GHC Core.
module RowDecoderGhcCore where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import Hpgsql.Encoding (FromPgField (..), FromPgRow (..), genericFromPgRow, singleField)

-- | BestCaseScenarioRecord's purpose is to have a very small row decoder in GHC Core
-- for my own understanding/comprehension of what a RowDecoder gets compiled to
-- in the best case scenario. Also, we expect one day to maybe reach a fully inlined
-- row decoder that only peeks at bytes and allocates 3 values per row (one
-- for each field), plus one `BestCaseScenarioRecord` per row.
-- In the GHC Core of this module (use `run ghc-core` to output it), it helps to:
-- - Look for the Record constructor and grep for it to find where the RowDecoder
--   invokes it, only to find where the RowDecoder is.
-- - Grep for numbers that exist in the decoders' implementation, such as 8#, 13#, 4#.
--   These are strong indicators that each decoder was inlined into the RowDecoder.
-- There still are unnecessary allocations/boxing even with full inlining, but maybe
-- one day we'll find a way to get rid of all of them.
data BestCaseScenarioRecord = BestCaseScenarioRecord
  { bcsId :: !Int,
    bcsDate :: !Day,
    bcsText :: !(Maybe Int)
  }

instance FromPgRow BestCaseScenarioRecord where
  rowDecoder = BestCaseScenarioRecord <$> inlinedSingleFieldRowDecoder <*> inlinedSingleFieldRowDecoder <*> inlinedSingleFieldRowDecoder

-- data BenchRow = BenchRow
--   { brId :: !Int,
--     brDate1 :: !Day,
--     brDate2 :: !Day,
--     brTimestamp1 :: !UTCTime,
--     brTimestamp2 :: !UTCTime,
--     brText1 :: !Text,
--     brText2 :: !Text,
--     brDouble1 :: !Double,
--     brDouble2 :: !Double,
--     brMaybeInt :: !(Maybe Int),
--     brMaybeText :: !(Maybe Text),
--     brMaybeDouble :: !(Maybe Double),
--     brMaybeDay :: !(Maybe Day)
--   }

-- Generically deriving section.

-- deriving instance Generic BenchRow

-- instance FromPgRow BenchRow where
--   rowDecoder = genericFromPgRow

-- Hand-written applicative style deriving section.
-- instance FromPgRow BenchRow where
--   rowDecoder =
--     SomeRecord
--       <$> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
--       <*> singleField fieldDecoder
