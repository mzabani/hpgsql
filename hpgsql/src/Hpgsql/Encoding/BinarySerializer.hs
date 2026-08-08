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
    decodePgFieldWithAtMost4Bytes,
    CoolWordDec(..)
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as InternalBS
import Data.Int (Int16, Int32, Int64)
import Prelude hiding (encodeFloat)
#if WORDS_BIGENDIAN
import Data.Word (Word16, Word32, Word64)
#else
import Data.Word (Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64, Word8)
#endif
import Data.Bits (Bits (unsafeShiftR))
import Data.Coerce (coerce)
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

data CoolWordDec a where
  CWord8 :: CoolWordDec Word8
  CWord16 :: CoolWordDec Word16
  CWord32 :: CoolWordDec Word32
  CWord64 :: CoolWordDec Word64

{-# INLINE decodeWord #-}
decodeWord :: CoolWordDec a -> ByteStringIdx -> ByteString -> (a -> a) -> Either String a
decodeWord wdec idx (InternalBS.BS bytesPtr len) endianConvert =
  case wdec of
    CWord8 -> if len < 1 + idx.idx then Left "Less than enough bytes to decode" else Right $ endianConvert $ unsafeDupablePerformIO $ withForeignPtr bytesPtr $ \ptr -> peekByteOff (coerce ptr) idx.idx
    CWord16 -> if len < 2 + idx.idx then Left "Less than enough bytes to decode" else Right $ endianConvert $ unsafeDupablePerformIO $ withForeignPtr bytesPtr $ \ptr -> peekByteOff (coerce ptr) idx.idx
    CWord32 -> if len < 4 + idx.idx then Left "Less than enough bytes to decode" else Right $ endianConvert $ unsafeDupablePerformIO $ withForeignPtr bytesPtr $ \ptr -> peekByteOff (coerce ptr) idx.idx
    CWord64 -> if len < 8 + idx.idx then Left "Less than enough bytes to decode" else Right $ endianConvert $ unsafeDupablePerformIO $ withForeignPtr bytesPtr $ \ptr -> peekByteOff (coerce ptr) idx.idx

{-# INLINE unsafeEncodeWord #-}
unsafeEncodeWord :: (Storable a) => a -> (a -> a) -> Int -> ByteString
unsafeEncodeWord n endianConvert len =
  InternalBS.unsafeCreate len $ \bufferPtr ->
    poke (coerce bufferPtr) $ endianConvert n

newtype ByteStringIdx = ByteStringIdx {idx :: Int}
  deriving newtype (Num)

{-# INLINE decodeInt16BE #-}
decodeInt16BE :: ByteStringIdx -> ByteString -> Either String Int16
decodeInt16BE idx bs = fromIntegral <$> decodeWord CWord16 idx bs fromBigEndian16

{-# INLINE encodeInt16BE #-}
encodeInt16BE :: Int16 -> ByteString
encodeInt16BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian16 2

{-# INLINE decodeWord8 #-}
decodeWord8 :: ByteStringIdx -> ByteString -> Either String Word8
decodeWord8 idx bs = decodeWord CWord8 idx bs Prelude.id

{-# INLINE decodeWord32BE #-}
decodeWord32BE :: ByteStringIdx -> ByteString -> Either String Word32
decodeWord32BE idx bs = decodeWord CWord32 idx bs fromBigEndian32

{-# INLINE decodeWord64BE #-}
decodeWord64BE :: ByteStringIdx -> ByteString -> Either String Word64
decodeWord64BE idx bs = decodeWord CWord64 idx bs fromBigEndian64

{-# INLINE decodeInt32BE #-}
decodeInt32BE :: ByteStringIdx -> ByteString -> Either String Int32
decodeInt32BE idx bs = fromIntegral <$> decodeWord CWord32 idx bs fromBigEndian32

{-# INLINE encodeInt32BE #-}
encodeInt32BE :: Int32 -> ByteString
encodeInt32BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian32 4

{-# INLINE decodeInt64BE #-}
decodeInt64BE :: ByteStringIdx -> ByteString -> Either String Int64
decodeInt64BE idx bs = fromIntegral <$> decodeWord CWord64 idx bs fromBigEndian64

{-# INLINE encodeInt64BE #-}
encodeInt64BE :: Int64 -> ByteString
encodeInt64BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian64 8

{-# INLINE encodeFloat #-}
encodeFloat :: Float -> ByteString
encodeFloat n = unsafeEncodeWord (castFloatToWord32 n) fromBigEndian32 4

{-# INLINE encodeDouble #-}
encodeDouble :: Double -> ByteString
encodeDouble n = unsafeEncodeWord (castDoubleToWord64 n) fromBigEndian64 8

-- TODO: Encode field length together with value for small types.
-- This can also be a performance boost by having fewer bytestrings?
{-# INLINE encodePgBoolean #-}
encodePgBoolean :: Bool -> ByteString
encodePgBoolean v = if v then "\SOH" else "\NUL"

{-# INLINE decodeDataRow #-}

-- | A super specialized decoder to decode a postgres DataRow message
-- more quickly than a naive implementation.
-- Returns the index into the left-unparsed contents of the supplied bytestring.
decodeDataRow :: ByteStringIdx -> ByteString -> Either String ByteStringIdx
decodeDataRow idx bs@(InternalBS.BS _bytesPtr len) =
  -- We have a fast path when rows are at least 8 bytes long (should be the case
  -- for all but 0-column query results or bytestring chunks "cut in the middle of the message")
  -- by playing with bitwise operations.
  -- Whether this is worth keeping is sort of questionable. It's complex
  -- (even if I think it's safe and well tested) and reduces runtime of one of
  -- our benchmarks by 2% compared to not having it.
  case decodeWord CWord64 idx bs fromBigEndian64 of
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
      if len >= 5 + idx.idx
        then do
          msgIdentChar <- decodeWord8 idx bs
          lenFullMsg <- decodeInt32BE (1 + idx) bs
          if msgIdentChar == 68 -- Letter 'D'
            then toResult (fromIntegral lenFullMsg)
            else Left "Not a DataRow"
        else Left "Less than enough bytes to decode a DataRow"
  where
    toResult lenFullMsg
      | len >= 1 + lenFullMsg + idx.idx = Right $ ByteStringIdx $ 1 + lenFullMsg + idx.idx
      | otherwise = Left "Less than enough bytes to decode a full DataRow"

{-# INLINE decodePgFieldWithAtMost4Bytes #-}

-- | A specialized decoder that decoders a query result's
-- field's contents, but only for PG fields at most 4 bytes long and
-- at least 1 byte long (so no text or void types, for example).
-- This includes essentially int32, int16, and booleans.
-- Pass in as type argument a Word8, Word16 or Word32 to indicate
-- the size of the PG type you're decoding.
-- Returns the index into the first yet-unparsed byte.
decodePgFieldWithAtMost4Bytes :: forall a. (Storable a, Integral a) => CoolWordDec a -> ByteStringIdx -> ByteString -> Either String (Maybe a, ByteStringIdx)
decodePgFieldWithAtMost4Bytes wdec =
  let (pgTypeSize, endianSwap, valueMask :: Word64) = case wdec of
        CWord8 -> (1, Prelude.id, 0b00000000_00000000_00000000_00000000_11111111_00000000_00000000_00000000)
        CWord16 -> (2, fromBigEndian16, 0b00000000_00000000_00000000_00000000_11111111_11111111_00000000_00000000)
        CWord32 -> (4, fromBigEndian32, 0b00000000_00000000_00000000_00000000_11111111_11111111_11111111_11111111)
        CWord64 -> error "Cannot decode 64 bits fields with this function. TODO: Make this function safer."
      valueShift :: Int = 8 * (4 - pgTypeSize)
   in \idx bs ->
        -- We try the most optimistic case first:
        -- - Non-null 4 byte long types (like int32)
        -- - Null int32 followed by at least one other field (not the last field in the row)
        -- - Shorter types (int16, bool) followed by at least one other field (not the last field in the row)
        -- In all the cases above, there are at least 8 bytes in the row, so our decoding into a Word64 will succeed.
        case decodeWord CWord64 idx bs fromBigEndian64 of
          Right (w64 :: Word64) ->
            let fieldLenW64 :: Word64 = flip unsafeShiftR 32 $ w64 .&. 0b11111111_11111111_11111111_11111111_00000000_00000000_00000000_00000000
                fieldIfNotNull :: a = fromIntegral $ unsafeShiftR (w64 .&. valueMask) valueShift
             in if fieldLenW64 == 0xFFFFFFFF -- (-1) in two's-complement
                  then
                    Right (Nothing, idx + 4)
                  else
                    if fieldLenW64 <= 4
                      then
                        Right (Just fieldIfNotNull, idx + 4 + fromIntegral fieldLenW64)
                      else Left "You cannot use decodePgFieldWithAtMost4Bytes to decode fields of types potentially more than 4 bytes long"
          Left _ -> do
            -- This is the not-as-optimistic case, which includes:
            -- - A NULL int32 as the last field in the row
            -- - A bool/int8/int16 that is the last field in the row
            lenField <- decodeInt32BE idx bs
            if lenField >= 0
              then do
                -- peek after the next 4 bytes for @a
                fieldValue <- decodeWord wdec (idx + 4) bs endianSwap
                Right (Just fieldValue, idx + 4 + fromIntegral lenField)
              else Right (Nothing, idx + 4)
