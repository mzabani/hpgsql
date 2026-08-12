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
  ( Parser (..),
    ParseResult (..),
    parseOnly,
    take,
    endOfInput,
    match,
    parseMany,
    matchLeftUnconsumed,
    takeInt16BE,
    takeInt32BE,
    takeInt64BE,
    takeDataRow,
    parseManyRows,
    skip,
    parsePgFieldWithAtMost4Bytes,
    takeInt64BEWithFieldLength,
    takeInt32BEWithFieldLength,
    takeInt16BEWithFieldLength,
    takeFloatBE,
    takeDoubleBE,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import Foreign.Storable (Storable)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Hpgsql.Encoding.BinarySerializer (ByteStringIdx (..))
import qualified Hpgsql.Encoding.BinarySerializer as BinSer
import Prelude hiding (take)

data ParseResult a
  = ParseFail !String
  | ParseOk !a
  deriving stock (Show)

-- | A parser that consumes a strict 'ByteString'.
newtype Parser a = Parser
  { unParser ::
      forall r.
      ByteStringIdx ->
      ByteString ->
      (String -> r) ->
      -- \^ failure continuation
      (a -> ByteStringIdx -> ByteString -> r) ->
      -- \^ success continuation, taking original or new ByteString, the index into the original/new bytestring of the first yet-unparsed byte, and parsed value
      r
  }

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
parseOnly :: Parser a -> ByteString -> ParseResult a
parseOnly p = parseOnlyOffset p 0
{-# INLINE parseOnly #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnlyOffset :: Parser a -> ByteStringIdx -> ByteString -> ParseResult a
parseOnlyOffset (Parser p) idx bs = p idx bs ParseFail (\a _ _ -> ParseOk a)
{-# INLINE parseOnlyOffset #-}

-- | Consume exactly @n@ bytes of input, failing if fewer than @n@ bytes
-- remain.
take :: Int -> Parser ByteString
take n = Parser $ \idx bs kf ks ->
  let skip' = n + idx.idx
   in if BS.length bs >= skip'
        then case BS.take n $ BS.drop idx.idx bs of
          -- Strict on the bytestring because we're pretty sure
          -- the field decoder will need to evaluate this anyway,
          -- so no need for an extra thunk
          !h -> ks h (ByteStringIdx skip') bs
        else kf ("take: wanted " <> show skip' <> " bytes but only " <> show (BS.length bs) <> " remain")
{-# INLINE take #-}

-- | Consume exactly @n@ bytes of input, failing if fewer than @n@ bytes
-- remain.
skip :: Int -> Parser ()
skip n = Parser $ \idx bs _ ks ->
  ks () (ByteStringIdx $ idx.idx + n) bs
{-# INLINE skip #-}

{-# INLINE takeInt16BE #-}
takeInt16BE :: Parser Int16
takeInt16BE = Parser $ \idx bs kf ks ->
  case BinSer.decodeInt16BE idx bs of
    Right v -> ks v (idx + 2) bs
    Left err -> kf err

{-# INLINE takeInt16BEWithFieldLength #-}

-- | Parses both a field length and the field itself, for
-- an Int16 in a row.
takeInt16BEWithFieldLength :: Parser (Maybe Int16)
takeInt16BEWithFieldLength = do
  mi16 <- parsePgFieldWithAtMost4Bytes BinSer.TypeSize2
  pure $ fromIntegral <$> mi16

{-# INLINE takeInt32BE #-}
takeInt32BE :: Parser Int32
takeInt32BE = Parser $ \idx bs kf ks ->
  case BinSer.decodeInt32BE idx bs of
    Right v -> ks v (idx + 4) bs
    Left err -> kf err

{-# INLINE takeInt32BEWithFieldLength #-}

-- | Parses both a field length and the field itself, for
-- an Int32 in a row.
takeInt32BEWithFieldLength :: Parser (Maybe Int32)
takeInt32BEWithFieldLength = do
  mi32 <- parsePgFieldWithAtMost4Bytes BinSer.TypeSize4
  pure $ fromIntegral <$> mi32

{-# INLINE takeFloatBE #-}
takeFloatBE :: Parser Float
takeFloatBE = Parser $ \idx bs kf ks ->
  case BinSer.decodeWord32BE idx bs of
    Right v -> ks (castWord32ToFloat v) (idx + 4) bs
    Left err -> kf err

{-# INLINE takeDoubleBE #-}
takeDoubleBE :: Parser Double
takeDoubleBE = Parser $ \idx bs kf ks ->
  case BinSer.decodeWord64BE idx bs of
    Right v -> ks (castWord64ToDouble v) (idx + 8) bs
    Left err -> kf err

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
  case BinSer.decodeInt64BE idx bs of
    Right v -> ks v (idx + 8) bs
    Left err -> kf err

{-# INLINE takeDataRow #-}

-- | A specialized parser to parse a postgres DataRow,
-- returning the index of the byte after this DataRow's last.
takeDataRow :: Parser ByteStringIdx
takeDataRow = Parser $ \idx bs kf ks ->
  case BinSer.decodeDataRow idx bs of
    Left err -> kf err
    Right idxRest -> ks idxRest idxRest bs

{-# INLINE parsePgFieldWithAtMost4Bytes #-}

-- | A specialized parser that reads a query result's
-- field's contents.
parsePgFieldWithAtMost4Bytes :: forall a. (Storable a, Integral a) => BinSer.WordDecoding a -> Parser (Maybe a)
parsePgFieldWithAtMost4Bytes wdec =
  let dec = BinSer.decodePgFieldWithAtMost4Bytes wdec
   in Parser $ \idx bs kf ks ->
        case dec idx bs of
          Right (v, restIdx) -> ks v restIdx bs
          Left err -> kf err

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \idx' bs' _kf ks -> let (vs, restIdx) = go idx' bs' in ks vs restIdx bs'
  where
    go idx bs = case parseOnlyOffset (matchLeftUnconsumed p) idx bs of
      ParseOk (unconsumedIdx, v) -> let (vs, rest) = go unconsumedIdx bs in (v : vs, rest)
      ParseFail _ -> ([], idx)
{-# INLINE parseMany #-}

parseManyRows :: Parser ByteStringIdx
parseManyRows = Parser $ \idx' bs' _kf ks -> let restIdx = go idx' bs' in ks restIdx restIdx bs'
  where
    go idx bs = case parseOnlyOffset takeDataRow idx bs of
      ParseOk unconsumedIdx -> go unconsumedIdx bs
      ParseFail _ -> idx
{-# INLINE parseManyRows #-}

-- | Succeeds only when the input has been fully consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \idx bs kf ks ->
  if BS.length bs <= idx.idx then ks () idx bs else kf "endOfInput: input remaining"
{-# INLINE endOfInput #-}

-- | Run a parser and additionally return the slice of input it consumed.
-- Because the input is a strict 'ByteString', the returned slice is a view
-- over the original buffer and allocates no extra memory.
match :: Parser a -> Parser (ByteString, a)
match (Parser p) = Parser $ \idx bs kf ks ->
  p
    idx
    bs
    kf
    ( \a idx' bs' ->
        let !consumed = BS.take (idx'.idx - idx.idx) $ BS.drop idx.idx bs
         in ks (consumed, a) idx' bs'
    )
{-# INLINE match #-}

-- | Run a parser and additionally return the index to the first unconsumed/unparsed byte
-- in the supplied ByteString.
matchLeftUnconsumed :: Parser a -> Parser (ByteStringIdx, a)
matchLeftUnconsumed (Parser p) = Parser $ \idx bs kf ks ->
  p
    idx
    bs
    kf
    ( \a idx' bs' ->
        ks (idx', a) idx' bs'
    )
{-# INLINE matchLeftUnconsumed #-}
