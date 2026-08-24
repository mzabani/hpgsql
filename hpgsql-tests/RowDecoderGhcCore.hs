{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

-- |
-- This is not a real test module. It's just a type deriving `FromPgRow`
-- so we can look at GHC Core output.
module RowDecoderGhcCore where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import Hpgsql.Encoding (FromPgRow (..), fieldDecoder, genericFromPgRow, singleField)

data BenchRow = BenchRow
  { brId :: !Int,
    brDate1 :: !Day,
    brDate2 :: !Day,
    brTimestamp1 :: !UTCTime,
    brTimestamp2 :: !UTCTime,
    brText1 :: !Text,
    brText2 :: !Text,
    brDouble1 :: !Double,
    brDouble2 :: !Double,
    brMaybeInt :: !(Maybe Int),
    brMaybeText :: !(Maybe Text),
    brMaybeDouble :: !(Maybe Double),
    brMaybeDay :: !(Maybe Day)
  }

-- Generically deriving section.

deriving instance Generic BenchRow

instance FromPgRow BenchRow where
  rowDecoder = genericFromPgRow

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
