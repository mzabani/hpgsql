{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

-- |
-- A minimal attoparsec-like parser that uses CPS to reduce allocations
-- and perform better than attoparsec, at least the way we use it in
-- hpgsql.
--
-- In benchmarks, this improved performance by 12-15% materializing
-- query results when it was introduced.
-- With the INLINE pragma in RowDecoder's Applicative's (<*>), GHC's
-- inliner was finally able to make full use of continuation passing,
-- and performance was improved by another ~14.3%, with total memory
-- allocations reduced by ~6%.
module Hpgsql.SimpleParser
  ( Parser,
    ParserPol (..),
    ParseResult (..),
    ParseResultRows (..),
    parseOnly,
    parseOnlyRows,
    take,
    endOfInput,
    takeInt16BE,
    takeInt32BE,
    takeInt64BE,
    parseManyRows,
    skip,
    takeInt64BEWithFieldLength,
    takeInt32BEWithFieldLength,
    takeInt16BEWithFieldLength,
    takeFloatBE,
    takeDoubleBE,
    takeFloatBEWithFieldLength,
    peekInt32BE,
    takeUtf8Text,
    takeWord64BE,
    takeWord8WithFieldLength,
  )
where

import Control.Applicative (Alternative (..))
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import GHC.Base (Int (..), Int#, LiftedRep, RuntimeRep (..), TYPE, (+#), (<=#), (>=#))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Hpgsql.Internal.PinnedByteArray (BoxedByteStringIdx (..), ByteStringIdx, PinnedByteArray)
import qualified Hpgsql.Internal.PinnedByteArray as PBA
import Prelude hiding (take)

data ParseResult a
  = ParseFail !String
  | ParseOk !a
  deriving stock (Show)

-- data ParseResultPol (rep :: RuntimeRep) (a :: TYPE rep)
--   = ParseFailPol !String
--   | ParseOkPol a
--   deriving stock (Show)

data ParseResultInt#
  = ParseFailInt# !String
  | ParseOkInt# {-# UNPACK #-} !Int#
  deriving stock (Show)

data ParseResultRows
  = ParseFailRows !String
  | ParseOkRows !(# ByteStringIdx, Int# #)

-- | A parser that consumes a strict 'PinnedByteArray'.
newtype ParserPol (rep :: RuntimeRep) (a :: TYPE rep) = Parser
  { unParser ::
      forall r.
      ByteStringIdx ->
      PinnedByteArray ->
      (String -> r) ->
      -- \^ failure continuation
      (a -> ByteStringIdx -> PinnedByteArray -> r) ->
      -- \^ success continuation, taking original or new PinnedByteArray, the index into the original/new bytestring of the first yet-unparsed byte, and parsed value
      r
  }

type Parser = ParserPol LiftedRep

instance Functor Parser where
  fmap f (Parser p) = Parser $ \idx bs kf ks ->
    p idx bs kf (\a bs' -> ks (f a) bs')
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \idx bs _ ks -> ks a idx bs
  {-# INLINE pure #-}

  Parser pf <*> Parser pa = Parser $ \idx bs kf ks ->
    pf idx bs kf (\f bs' idx' -> pa bs' idx' kf (\a bs'' idx'' -> ks (f a) bs'' idx''))
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = fail "empty Alternative"
  {-# INLINE empty #-}
  Parser p1 <|> Parser p2 = Parser $ \idx bs kf ks ->
    p1 idx bs (\_ -> p2 idx bs kf ks) ks
  {-# INLINE (<|>) #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}

  Parser p >>= k = Parser $ \idx bs kf ks ->
    p idx bs kf (\a bs' idx' -> unParser (k a) bs' idx' kf ks)
  {-# INLINE (>>=) #-}

instance MonadFail Parser where
  fail msg = Parser $ \_ _ kf _ -> kf msg
  {-# INLINE fail #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnly :: Parser a -> PinnedByteArray -> ParseResult a
parseOnly p = parseOnlyOffset p 0#
{-# INLINE parseOnly #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnlyRows :: ParserPol ('TupleRep [IntRep, IntRep]) (# ByteStringIdx, Int# #) -> PinnedByteArray -> ParseResultRows
parseOnlyRows p = parseOnlyOffsetRows p 0#
{-# INLINE parseOnlyRows #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnlyOffset :: Parser a -> ByteStringIdx -> PinnedByteArray -> ParseResult a
parseOnlyOffset (Parser p) idx bs = p idx bs ParseFail (\a _ _ -> ParseOk a)
{-# INLINE parseOnlyOffset #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnlyOffsetInt# :: ParserPol 'IntRep Int# -> ByteStringIdx -> PinnedByteArray -> ParseResultInt#
parseOnlyOffsetInt# (Parser p) idx bs = p idx bs ParseFailInt# (\a _ _ -> ParseOkInt# a)
{-# INLINE parseOnlyOffsetInt# #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnlyOffsetRows :: ParserPol ('TupleRep [IntRep, IntRep]) (# ByteStringIdx, Int# #) -> ByteStringIdx -> PinnedByteArray -> ParseResultRows
parseOnlyOffsetRows (Parser p) idx bs = p idx bs ParseFailRows (\a _ _ -> ParseOkRows a)
{-# INLINE parseOnlyOffsetRows #-}

-- | Consume exactly `n` bytes of input, failing if fewer than `n` bytes
-- remain.
take :: Int -> Parser PinnedByteArray
take bn@(I# n) = Parser $ \idx sbs kf ks ->
  let skip' = n +# idx
   in case PBA.length sbs >=# skip' of
        -- TODO: dropAndTake in a single call
        1# -> case PBA.take bn $ PBA.drop (I# idx) sbs of
          -- Strict on the bytestring because we're pretty sure
          -- the field decoder will need to evaluate this anyway,
          -- so no need for an extra thunk
          !h -> ks h skip' sbs
        _ -> kf "take: insufficient bytes"
{-# INLINE take #-}

-- | Consume exactly `n` bytes of input, failing if fewer than `n` bytes
-- remain, and assumes those next `n` bytes are UTF8 text, so returns them
-- as `Text`.
takeUtf8Text :: Int -> Parser Text
takeUtf8Text bn@(I# n) = Parser $ \idx sbs kf ks ->
  let skip' = n +# idx
   in case PBA.length sbs >=# skip' of
        1# -> case PBA.unsafeToUtf8Text idx bn sbs of
          Left err -> kf err
          Right t -> ks t skip' sbs
        _ -> kf ("take: wanted " <> show (I# skip') <> " bytes but only " <> show (I# $ PBA.length sbs) <> " remain")
{-# INLINE takeUtf8Text #-}

-- | Consume exactly @n@ bytes of input, failing if fewer than @n@ bytes
-- remain.
skip :: Int -> Parser ()
skip (I# n) = Parser $ \idx bs _ ks ->
  ks () (idx +# n) bs
{-# INLINE skip #-}

{-# INLINE takeInt16BE #-}
takeInt16BE :: Parser Int16
takeInt16BE = Parser $ \idx bs kf ks ->
  case PBA.decodeInt16BE idx bs of
    Right v -> ks v (idx +# 2#) bs
    Left err -> kf err

{-# INLINE takeInt32BE #-}
takeInt32BE :: Parser Int32
takeInt32BE = Parser $ \idx bs kf ks ->
  case PBA.decodeInt32BE idx bs of
    Right v -> ks v (idx +# 4#) bs
    Left err -> kf err

{-# INLINE peekInt32BE #-}
peekInt32BE :: Parser Int32
peekInt32BE = Parser $ \idx bs kf ks ->
  case PBA.decodeInt32BE idx bs of
    Right v -> ks v idx bs
    Left err -> kf err

{-# INLINE takeFloatBEWithFieldLength #-}

-- | Parses both a field length and the field itself, for
-- a Float in a row.
takeFloatBEWithFieldLength :: Parser (Maybe Float)
takeFloatBEWithFieldLength = do
  len <- takeInt32BE
  case len of
    (-1) -> pure Nothing
    _ -> Just . castWord32ToFloat <$> takeWord32BE

{-# INLINE takeFloatBE #-}
takeFloatBE :: Parser Float
takeFloatBE = Parser $ \idx bs kf ks ->
  case PBA.decodeWord32BE idx bs of
    Right v -> ks (castWord32ToFloat v) (idx +# 4#) bs
    Left err -> kf err

{-# INLINE takeDoubleBE #-}
takeDoubleBE :: Parser Double
takeDoubleBE = Parser $ \idx bs kf ks ->
  case PBA.decodeWord64BE idx bs of
    Right v -> ks (castWord64ToDouble v) (idx +# 8#) bs
    Left err -> kf err

{-# INLINE takeWord8 #-}
takeWord8 :: Parser Word8
takeWord8 = Parser $ \idx bs kf ks ->
  case PBA.decodeWord8 idx bs of
    Right v -> ks v (idx +# 1#) bs
    Left err -> kf err

{-# INLINE takeWord8WithFieldLength #-}
takeWord8WithFieldLength :: Parser (Maybe Word8)
takeWord8WithFieldLength = do
  fieldLen <- takeInt32BE
  if fieldLen == (-1)
    then pure Nothing
    else Just <$> takeWord8

{-# INLINE takeInt16BEWithFieldLength #-}

-- | Parses both a field length and the field itself, for
-- an Int16 in a row.
takeInt16BEWithFieldLength :: Parser (Maybe Int16)
takeInt16BEWithFieldLength = do
  fieldLen <- takeInt32BE
  if fieldLen == (-1)
    then pure Nothing
    else Just <$> takeInt16BE

{-# INLINE takeInt32BEWithFieldLength #-}

-- | Parses both a field length and the field itself, for
-- an Int32 in a row.
takeInt32BEWithFieldLength :: Parser (Maybe Int32)
takeInt32BEWithFieldLength = do
  fieldLen <- takeInt32BE
  if fieldLen == (-1)
    then pure Nothing
    else Just <$> takeInt32BE

{-# INLINE takeInt64BEWithFieldLength #-}

-- | Parses both a field length and the field itself, for
-- an Int64 in a row.
takeInt64BEWithFieldLength :: Parser (Maybe Int64)
takeInt64BEWithFieldLength = do
  fieldLen <- takeInt32BE
  if fieldLen == (-1)
    then pure Nothing
    else Just <$> takeInt64BE

{-# INLINE takeInt64BE #-}
takeInt64BE :: Parser Int64
takeInt64BE = Parser $ \idx bs kf ks ->
  case PBA.decodeInt64BE idx bs of
    Right v -> ks v (idx +# 8#) bs
    Left err -> kf err

{-# INLINE takeWord32BE #-}
takeWord32BE :: Parser Word32
takeWord32BE = Parser $ \idx bs kf ks ->
  case PBA.decodeWord32BE idx bs of
    Right v -> ks v (idx +# 4#) bs
    Left err -> kf err

{-# INLINE takeWord64BE #-}
takeWord64BE :: Parser Word64
takeWord64BE = Parser $ \idx bs kf ks ->
  case PBA.decodeWord64BE idx bs of
    Right v -> ks v (idx +# 8#) bs
    Left err -> kf err

{-# INLINE takeDataRow #-}

-- | A specialized parser to parse a postgres DataRow,
-- returning the index of the byte after this DataRow's last.
takeDataRow :: ParserPol 'IntRep ByteStringIdx
takeDataRow = Parser $ \idx bs kf ks ->
  case PBA.decodeDataRow idx bs of
    Left err -> kf err
    Right (BoxedByteStringIdx (I# idxRest#)) -> ks idxRest# idxRest# bs

-- | Parses as many PG rows as there are available, returns
-- the index/offset of the first left-unparsed byte and the
-- number of rows parsed.
parseManyRows :: ParserPol ('TupleRep [IntRep, IntRep]) (# ByteStringIdx, Int# #)
parseManyRows = Parser $ \idx' bs' _kf ks -> case go idx' bs' 0# of
  (# restIdx#, nParsed #) -> ks (# restIdx#, nParsed #) restIdx# bs'
  where
    go idx# bs !nParsedSoFar = case parseOnlyOffsetInt# takeDataRow idx# bs of
      ParseOkInt# unconsumedIdx -> go unconsumedIdx bs (nParsedSoFar +# 1#)
      ParseFailInt# _ -> (# idx#, nParsedSoFar #)
{-# INLINE parseManyRows #-}

-- | Succeeds only when the input has been fully consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \idx bs kf ks ->
  case PBA.length bs <=# idx of
    1# -> ks () idx bs
    _ -> kf "endOfInput: input remaining"
{-# INLINE endOfInput #-}
