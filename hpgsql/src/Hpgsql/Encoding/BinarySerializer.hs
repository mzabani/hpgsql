{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}

-- |
-- A replacement for libraries like cereal or binary.
-- In our tests, this is ~4.7% faster than cereal, and it also
-- (or by virtue of) allocates ~13% less memory in some of our benchmarks.
-- And it also means one fewer dependency.
module Hpgsql.Encoding.BinarySerializer
  ( decodeInt16BE,
    decodeInt32BE,
    decodeInt64BE,
    decodeWord32BE,
    decodeWord64BE,
    encodeInt32BE,
    encodeDouble,
    encodeFloat,
    encodeInt64BE,
    encodeInt16BE,
    encodePgBoolean,
    decodeDataRow,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as InternalBS
import Data.Int (Int16, Int32, Int64)
import Prelude hiding (encodeFloat)
#if WORDS_BIGENDIAN
import Data.Word (Word16, Word32, Word64)
#else
import Data.Word (Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
#endif
import Data.Bits (Bits (unsafeShiftR))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Foreign (Storable (..), peek, (.&.))
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.IO.Unsafe (unsafeDupablePerformIO)

fromBigEndian32 :: Word32 -> Word32
#if WORDS_BIGENDIAN
fromBigEndian32 = Prelude.id
#else
fromBigEndian32 = byteSwap32
#endif

fromBigEndian64 :: Word64 -> Word64
#if WORDS_BIGENDIAN
fromBigEndian64 = Prelude.id
#else
fromBigEndian64 = byteSwap64
#endif

fromBigEndian16 :: Word16 -> Word16
#if WORDS_BIGENDIAN
fromBigEndian16 = Prelude.id
#else
fromBigEndian16 = byteSwap16
#endif

unsafeDecodeWord :: (Storable a) => ByteString -> Int -> (a -> a) -> Either String a
unsafeDecodeWord (InternalBS.BS bytesPtr len) minLen endianConvert =
  if len >= minLen
    then
      -- A bang (strictness) in `decodedWord` makes our benchmarks allocate more memory and run slower!
      let decodedWord = endianConvert $ unsafeDupablePerformIO $ withForeignPtr bytesPtr $ \ptr -> peek (coerce ptr)
       in Right decodedWord
    else Left "Less than enough bytes to decode"

unsafeEncodeWord :: (Storable a) => a -> (a -> a) -> Int -> ByteString
unsafeEncodeWord n endianConvert len =
  InternalBS.unsafeCreate len $ \bufferPtr ->
    poke (coerce bufferPtr) $ endianConvert n

decodeInt16BE :: ByteString -> Either String Int16
decodeInt16BE bs = fromIntegral <$> unsafeDecodeWord bs 2 fromBigEndian16

encodeInt16BE :: Int16 -> ByteString
encodeInt16BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian16 2

decodeWord32BE :: ByteString -> Either String Word32
decodeWord32BE bs = unsafeDecodeWord bs 4 fromBigEndian32

decodeWord64BE :: ByteString -> Either String Word64
decodeWord64BE bs = unsafeDecodeWord bs 8 fromBigEndian64

decodeInt32BE :: ByteString -> Either String Int32
decodeInt32BE bs = fromIntegral <$> unsafeDecodeWord bs 4 fromBigEndian32

encodeInt32BE :: Int32 -> ByteString
encodeInt32BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian32 4

decodeInt64BE :: ByteString -> Either String Int64
decodeInt64BE bs = fromIntegral <$> unsafeDecodeWord bs 8 fromBigEndian64

encodeInt64BE :: Int64 -> ByteString
encodeInt64BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian64 8

encodeFloat :: Float -> ByteString
encodeFloat n = unsafeEncodeWord (castFloatToWord32 n) fromBigEndian32 4

encodeDouble :: Double -> ByteString
encodeDouble n = unsafeEncodeWord (castDoubleToWord64 n) fromBigEndian64 8

encodePgBoolean :: Bool -> ByteString
encodePgBoolean v = if v then "\SOH" else "\NUL"

{-# INLINE decodeDataRow #-}

-- | A super specialized decoder to decode a postgres DataRow message
-- more quickly than a naive implementation.
-- Returns first the parsed DataRow (only column sizes and values) and second
-- the left-unparsed original bytestring.
decodeDataRow :: ByteString -> Either String (ByteString, ByteString)
decodeDataRow bs@(InternalBS.BS _bytesPtr len) =
  -- We have a fast path when rows are at least 8 bytes long (should be the case
  -- for all but 0-column query results or bytestring chunks "cut in the middle of the message")
  -- by playing with bitwise operations.
  -- Whether this is worth keeping is sort of questionable. It's complex
  -- (even if I think it's safe and well tested) and reduces runtime of one of
  -- our benchmarks by 2% compared to not having it.
  case unsafeDecodeWord bs 8 fromBigEndian64 of
    Right (w64 :: Word64) ->
      -- After fromBigEndian64, the Word64 has bytes in big-endian order:
      -- byte 0 (msg type) in MSB, bytes 1-4 (length) next, bytes 5-6 (col count), byte 7 in LSB.
      let msgIdentByte64 = w64 .&. 0b11111111_00000000_00000000_00000000_00000000_00000000_00000000_00000000
          lenFullMsg = flip unsafeShiftR 24 $ w64 .&. 0b00000000_11111111_11111111_11111111_11111111_00000000_00000000_00000000
          letterD :: Word64 = 0b01000100_00000000_00000000_00000000_00000000_00000000_00000000_00000000
       in if msgIdentByte64 == letterD
            then
              toResult (fromIntegral lenFullMsg)
            else Left "Not a DataRow (Word64 bits decoding path)"
    Left _ ->
      -- It is possible the DataRow has length less than 8 bytes, so
      -- we still have to try to parse that.
      if len >= 5
        then
          let (InternalBS.w2c -> msgIdentChar, lenbs) = fromMaybe (error "impossible") $ BS.uncons bs
              lenFullMsg = fromIntegral $ either error id (decodeInt32BE lenbs)
           in if msgIdentChar == 'D'
                then toResult lenFullMsg
                else Left "Not a DataRow"
        else Left "Less than enough bytes to decode a DataRow"
  where
    toResult lenFullMsg
      | len >= 1 + lenFullMsg = let (a, rest) = BS.splitAt (1 + lenFullMsg) bs in Right (BS.drop 7 a, rest)
      | otherwise = Left "Less than enough bytes to decode a full DataRow"
