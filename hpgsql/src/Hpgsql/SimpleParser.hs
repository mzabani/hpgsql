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
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
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
  -- Special-casing n>0 helps reduce memory usage
  -- by ~1.5% in our behmarks without a measurable
  -- difference in run time
  if n > 0
    then
      let skip = n + idx.idx
       in if BS.length bs >= skip
            then case BS.take n $ BS.drop idx.idx bs of
              !h -> ks h (ByteStringIdx skip) bs
            else kf ("take: wanted " <> show skip <> " bytes but only " <> show (BS.length bs) <> " remain")
    else
      ks mempty idx bs
{-# INLINE take #-}

{-# INLINE takeInt16BE #-}
takeInt16BE :: Parser Int16
takeInt16BE = Parser $ \idx bs kf ks ->
  case BinSer.decodeInt16BE idx bs of
    Left err -> kf err
    Right v -> ks v (idx + 2) bs

{-# INLINE takeInt32BE #-}
takeInt32BE :: Parser Int32
takeInt32BE = Parser $ \idx bs kf ks ->
  case BinSer.decodeInt32BE idx bs of
    Left err -> kf err
    Right v -> ks v (idx + 4) bs

{-# INLINE takeInt64BE #-}
takeInt64BE :: Parser Int64
takeInt64BE = Parser $ \idx bs kf ks ->
  case BinSer.decodeInt64BE idx bs of
    Left err -> kf err
    Right v -> ks v (idx + 8) bs

{-# INLINE takeDataRow #-}

-- | A specialized parser to parse a postgres DataRow.
takeDataRow :: Parser ByteString
takeDataRow = Parser $ \idx bs kf ks ->
  case BinSer.decodeDataRow idx bs of
    Left err -> kf err
    Right (thisDataRow, idxRest) -> ks thisDataRow idxRest bs

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \bs' idx _kf ks -> let (vs, rest) = go bs' idx in ks vs 0 rest
  where
    go idx bs = case parseOnlyOffset (matchLeftUnconsumed p) idx bs of
      ParseOk (unconsumed, v) -> let (vs, rest) = go 0 unconsumed in (v : vs, rest)
      ParseFail _ -> ([], bs)
{-# INLINE parseMany #-}

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

-- | Run a parser and additionally return the unconsumed/unparsed ByteString.
matchLeftUnconsumed :: Parser a -> Parser (ByteString, a)
matchLeftUnconsumed (Parser p) = Parser $ \idx bs kf ks ->
  p
    idx
    bs
    kf
    ( \a idx' bs' ->
        ks (BS.drop idx'.idx bs', a) idx' bs'
    )
{-# INLINE matchLeftUnconsumed #-}
