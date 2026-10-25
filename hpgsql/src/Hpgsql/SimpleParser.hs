{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-coercions -dsuppress-module-prefixes -dsuppress-type-applications -ddump-to-file #-}

-- |
-- An attoparsec-like parser for 'PinnedByteArray'.
module Hpgsql.SimpleParser
  ( Parser,
    ParseResult (..),
    parseOnly,
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
import GHC.Base (Int (..), Int#, (+#), (<=#), (>=#))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Hpgsql.Internal.PinnedByteArray (ByteStringIdx, EitherByteStringIdx (..), PinnedByteArray)
import qualified Hpgsql.Internal.PinnedByteArray as PBA
import Prelude hiding (take)

data ParseResult a
  = ParseFail !String
  | ParseOk !a !ByteStringIdx
  deriving stock (Show)

-- | A parser that consumes a strict 'PinnedByteArray'.
newtype Parser a = Parser
  { unParser ::
      ByteStringIdx ->
      PinnedByteArray ->
      ParseResult a
      -- \^ either a failure message, or the parsed value along with the
      -- index into the original pinnedbytearray of the first yet-unparsed
      -- byte
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \idx bs -> case p idx bs of
    ParseFail err -> ParseFail err
    ParseOk a idx' -> ParseOk (f a) idx'
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \idx _ -> ParseOk a idx
  {-# INLINE pure #-}

  Parser pf <*> Parser pa = Parser $ \idx bs -> case pf idx bs of
    ParseFail err -> ParseFail err
    ParseOk f idx' -> case pa idx' bs of
      ParseFail err -> ParseFail err
      ParseOk a idx'' -> ParseOk (f a) idx''
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = fail "empty Alternative"
  {-# INLINE empty #-}
  Parser p1 <|> Parser p2 = Parser $ \idx bs -> case p1 idx bs of
    ParseFail _ -> p2 idx bs
    r -> r
  {-# INLINE (<|>) #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}

  Parser p >>= k = Parser $ \idx bs -> case p idx bs of
    ParseFail err -> ParseFail err
    ParseOk a idx' -> unParser (k a) idx' bs
  {-# INLINE (>>=) #-}

instance MonadFail Parser where
  fail msg = Parser $ \_ _ -> ParseFail msg
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
parseOnlyOffset :: Parser a -> ByteStringIdx -> PinnedByteArray -> ParseResult a
parseOnlyOffset (Parser p) idx bs = p idx bs
{-# INLINE parseOnlyOffset #-}

-- | Consume exactly `n` bytes of input, failing if fewer than `n` bytes
-- remain.
take :: Int -> Parser PinnedByteArray
take bn@(I# n) = Parser $ \idx sbs ->
  let !(skip' :: Int#) = n +# idx
   in case PBA.length sbs >=# skip' of
        -- TODO: dropAndTake in a single call
        1# -> case PBA.take bn $ PBA.drop (I# idx) sbs of
          -- Strict on the bytestring because we're pretty sure
          -- the field decoder will need to evaluate this anyway,
          -- so no need for an extra thunk
          !h -> ParseOk h skip'
        _ -> ParseFail "take: insufficient bytes"
{-# INLINE take #-}

-- | Consume exactly `n` bytes of input, failing if fewer than `n` bytes
-- remain, and assumes those next `n` bytes are UTF8 text, so returns them
-- as `Text`.
takeUtf8Text :: Int -> Parser Text
takeUtf8Text bn@(I# n) = Parser $ \idx sbs ->
  let skip' = n +# idx
   in case PBA.length sbs >=# skip' of
        1# -> case PBA.unsafeToUtf8Text idx bn sbs of
          Left err -> ParseFail err
          Right t -> ParseOk t skip'
        _ -> ParseFail ("take: wanted " <> show (I# skip') <> " bytes but only " <> show (I# $ PBA.length sbs) <> " remain")
{-# INLINE takeUtf8Text #-}

-- | Consume exactly @n@ bytes of input, failing if fewer than @n@ bytes
-- remain.
skip :: Int -> Parser ()
skip (I# n) = Parser $ \idx _ ->
  ParseOk () (idx +# n)
{-# INLINE skip #-}

{-# INLINE takeInt16BE #-}
takeInt16BE :: Parser Int16
takeInt16BE = Parser $ \idx bs -> case PBA.decodeInt16BE idx bs of
  Right v -> ParseOk v (idx +# 2#)
  Left err -> ParseFail err

{-# INLINE takeInt32BE #-}
takeInt32BE :: Parser Int32
takeInt32BE = Parser $ \idx bs -> case PBA.decodeInt32BE idx bs of
  Right v -> ParseOk v (idx +# 4#)
  Left err -> ParseFail err

{-# INLINE peekInt32BE #-}
peekInt32BE :: Parser Int32
peekInt32BE = Parser $ \idx bs -> case PBA.decodeInt32BE idx bs of
  Right v -> ParseOk v idx
  Left err -> ParseFail err

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
takeFloatBE = Parser $ \idx bs -> case PBA.decodeWord32BE idx bs of
  Right v -> ParseOk (castWord32ToFloat v) (idx +# 4#)
  Left err -> ParseFail err

{-# INLINE takeDoubleBE #-}
takeDoubleBE :: Parser Double
takeDoubleBE = Parser $ \idx bs -> case PBA.decodeWord64BE idx bs of
  Right v -> ParseOk (castWord64ToDouble v) (idx +# 8#)
  Left err -> ParseFail err

{-# INLINE takeWord8 #-}
takeWord8 :: Parser Word8
takeWord8 = Parser $ \idx bs -> case PBA.decodeWord8 idx bs of
  Right v -> ParseOk v (idx +# 1#)
  Left err -> ParseFail err

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
takeInt64BE = Parser $ \idx bs -> case PBA.decodeInt64BE idx bs of
  Right v -> ParseOk v (idx +# 8#)
  Left err -> ParseFail err

{-# INLINE takeWord32BE #-}
takeWord32BE :: Parser Word32
takeWord32BE = Parser $ \idx bs -> case PBA.decodeWord32BE idx bs of
  Right v -> ParseOk v (idx +# 4#)
  Left err -> ParseFail err

{-# INLINE takeWord64BE #-}
takeWord64BE :: Parser Word64
takeWord64BE = Parser $ \idx bs -> case PBA.decodeWord64BE idx bs of
  Right v -> ParseOk v (idx +# 8#)
  Left err -> ParseFail err

{-# INLINE takeDataRow #-}

-- | A specialized parser to parse a postgres DataRow,
-- returning the index of the byte after this DataRow's last.
takeDataRow :: Parser ()
takeDataRow = Parser $ \idx bs -> case PBA.decodeDataRow idx bs of
  LeftB err -> ParseFail err
  RightB idxRest -> ParseOk () idxRest

-- | Parses as many PG rows as there are available, returns
-- the index/offset of the first left-unparsed byte and the
-- number of rows parsed.
parseManyRows :: Parser Int
parseManyRows = Parser $ \idx' bs' -> go idx' bs' 0
  where
    go idx# bs !(nParsedSoFar :: Int) = case parseOnlyOffset takeDataRow idx# bs of
      ParseOk () unconsumedIdx -> go unconsumedIdx bs (nParsedSoFar + 1)
      ParseFail _ -> ParseOk nParsedSoFar idx#
{-# INLINE parseManyRows #-}

-- | Succeeds only when the input has been fully consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \idx bs ->
  case PBA.length bs <=# idx of
    1# -> ParseOk () idx
    _ -> ParseFail "endOfInput: input remaining"
{-# INLINE endOfInput #-}
