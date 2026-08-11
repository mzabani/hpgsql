{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}

-- |
-- A replacement for libraries like cereal or binary.
-- In our tests, this is ~6.0% faster than cereal, and it also
-- (or by virtue of) allocates ~13% less memory in some of our benchmarks.
-- And it also means one fewer dependency.
-- The caveat is that this module makes unaligned memory access. For the target
-- CPU architectures of this library, this should be fine.
module Hpgsql.Encoding.BinarySerializer
  ( ByteStringIdx (..),
    decodeInt16BE,
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
import Foreign (Storable (..), (.&.))
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

{-# INLINE unsafeDecodeWord #-}
unsafeDecodeWord :: (Storable a) => ByteStringIdx -> ByteString -> Int -> (a -> a) -> Either String a
unsafeDecodeWord idx (InternalBS.BS bytesPtr len) minLen endianConvert =
  if len >= minLen + idx.idx
    then
      -- A bang (strictness) in `decodedWord` makes our benchmarks allocate more memory and run slower!
      let decodedWord = endianConvert $ unsafeDupablePerformIO $ withForeignPtr bytesPtr $ \ptr -> peekByteOff (coerce ptr) idx.idx
       in Right decodedWord
    else Left "Less than enough bytes to decode"

{-# INLINE unsafeEncodeWord #-}
unsafeEncodeWord :: (Storable a) => a -> (a -> a) -> Int -> ByteString
unsafeEncodeWord n endianConvert len =
  InternalBS.unsafeCreate len $ \bufferPtr ->
    poke (coerce bufferPtr) $ endianConvert n

newtype ByteStringIdx = ByteStringIdx {idx :: Int}
  deriving newtype (Num)

{-# INLINE decodeInt16BE #-}
decodeInt16BE :: ByteStringIdx -> ByteString -> Either String Int16
decodeInt16BE idx bs = fromIntegral <$> unsafeDecodeWord idx bs 2 fromBigEndian16

{-# INLINE encodeInt16BE #-}
encodeInt16BE :: Int16 -> ByteString
encodeInt16BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian16 2

{-# INLINE decodeWord32BE #-}
decodeWord32BE :: ByteString -> Either String Word32
decodeWord32BE bs = unsafeDecodeWord 0 bs 4 fromBigEndian32

{-# INLINE decodeWord64BE #-}
decodeWord64BE :: ByteString -> Either String Word64
decodeWord64BE bs = unsafeDecodeWord 0 bs 8 fromBigEndian64

{-# INLINE decodeInt32BE #-}
decodeInt32BE :: ByteStringIdx -> ByteString -> Either String Int32
decodeInt32BE idx bs = fromIntegral <$> unsafeDecodeWord idx bs 4 fromBigEndian32

{-# INLINE encodeInt32BE #-}
encodeInt32BE :: Int32 -> ByteString
encodeInt32BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian32 4

{-# INLINE decodeInt64BE #-}
decodeInt64BE :: ByteStringIdx -> ByteString -> Either String Int64
decodeInt64BE idx bs = fromIntegral <$> unsafeDecodeWord idx bs 8 fromBigEndian64

{-# INLINE encodeInt64BE #-}
encodeInt64BE :: Int64 -> ByteString
encodeInt64BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian64 8

{-# INLINE encodeFloat #-}
encodeFloat :: Float -> ByteString
encodeFloat n = unsafeEncodeWord (castFloatToWord32 n) fromBigEndian32 4

{-# INLINE encodeDouble #-}
encodeDouble :: Double -> ByteString
encodeDouble n = unsafeEncodeWord (castDoubleToWord64 n) fromBigEndian64 8

{-# INLINE encodePgBoolean #-}
encodePgBoolean :: Bool -> ByteString
encodePgBoolean v = if v then "\SOH" else "\NUL"

{-# INLINE decodeDataRow #-}

-- | A super specialized decoder to decode a postgres DataRow message
-- more quickly than a naive implementation.
-- Returns first the parsed DataRow (only column sizes and values) and second
-- the index into the left-unparsed contents of the supplied bytestring.
decodeDataRow :: ByteStringIdx -> ByteString -> Either String (ByteString, ByteStringIdx)
decodeDataRow idx bs@(InternalBS.BS _bytesPtr len) =
  -- We have a fast path when rows are at least 8 bytes long (should be the case
  -- for all but 0-column query results or bytestring chunks "cut in the middle of the message")
  -- by playing with bitwise operations.
  -- Whether this is worth keeping is sort of questionable. It's complex
  -- (even if I think it's safe and well tested) and reduces runtime of one of
  -- our benchmarks by 2% compared to not having it.
  case unsafeDecodeWord idx bs 8 fromBigEndian64 of
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
          -- TODO: Don't allocate "lenbs" and decodeInt32BE with offset=1?
          let (InternalBS.w2c -> msgIdentChar, lenbs) = fromMaybe (error "impossible") $ BS.uncons $ BS.drop idx.idx bs
              lenFullMsg = fromIntegral $ either error id (decodeInt32BE 0 lenbs)
           in if msgIdentChar == 'D'
                then toResult lenFullMsg
                else Left "Not a DataRow"
        else Left "Less than enough bytes to decode a DataRow"
  where
    toResult lenFullMsg
      | len >= 1 + lenFullMsg + idx.idx = let a = BS.take (lenFullMsg - 6) (BS.drop (7 + idx.idx) bs) in Right (a, ByteStringIdx $ 1 + lenFullMsg + idx.idx)
      | otherwise = Left "Less than enough bytes to decode a full DataRow"
