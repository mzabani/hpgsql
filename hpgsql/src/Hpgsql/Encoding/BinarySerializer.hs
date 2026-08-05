{-# LANGUAGE CPP #-}

-- |
-- A replacement for libraries like cereal or binary.
-- In our tests, this is ~X% faster than cereal, and it also
-- allocates ~Y% less memory in some of our benchmarks.
-- It also means one fewer dependency.
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
import Data.Coerce (coerce)
import Foreign (Storable (..), peek)
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

decodeWord32BE :: ByteString -> Either String Word32
decodeWord32BE bs = unsafeDecodeWord bs 4 fromBigEndian32

decodeWord64BE :: ByteString -> Either String Word64
decodeWord64BE bs = unsafeDecodeWord bs 8 fromBigEndian64

decodeInt32BE :: ByteString -> Either String Int32
decodeInt32BE bs = fromIntegral <$> unsafeDecodeWord bs 2 fromBigEndian32

encodeInt32BE :: Int32 -> ByteString
encodeInt32BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian32 4

decodeInt64BE :: ByteString -> Either String Int64
decodeInt64BE bs = fromIntegral <$> unsafeDecodeWord bs 2 fromBigEndian64

encodeInt64BE :: Int64 -> ByteString
encodeInt64BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian64 8

encodeFloat :: Float -> ByteString
encodeFloat n = unsafeEncodeWord (castFloatToWord32 n) fromBigEndian32 4

encodeDouble :: Double -> ByteString
encodeDouble n = unsafeEncodeWord (castDoubleToWord64 n) fromBigEndian64 8
