{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

-- |
-- This is not a real test module. It's just a type deriving `FromPgRow`
-- so we can look at GHC Core output. It's as small as we can make it to
-- facilitate reading GHC Core.
module RowDecoderGhcCore where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import Hpgsql.Encoding (FromPgRow (..), fieldDecoder, genericFromPgRow, singleField, singleFieldRowDecoder)

-- | SmallRecord's purpose is to have a very small row decoder in GHC Core
-- for understanding. Also, we expect one day to maybe reach a fully inlined
-- row decoder that only peeks at bytes and allocates 3 values per row (one
-- for each field), plus one `SmallRecord` per row.
-- If we can get there, it'd be fabulous.
data SmallRecord = SmallRecord
  { smallId :: !Int,
    smallDate :: !Day,
    smallText :: !Int
  }

instance FromPgRow SmallRecord where
  rowDecoder = SmallRecord <$> singleFieldRowDecoder <*> singleFieldRowDecoder <*> singleFieldRowDecoder

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
