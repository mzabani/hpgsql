{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Why our own `PinnedByteArray` type instead of just using `ByteString`?
-- It all started when upon inspecting our row decoder's GHC Core, I saw
-- `lazy`, `keepAlive` and boxing+unboxing of Word32s that seemed completely
-- unnecessary. Claude suggested `lazy` - which appeared in GHC Core - acted
-- like an optimization fence, and I don't remember the details now, but
-- basically a `ByteString` uses a `ForeignPtr` under the hood, which requires
-- `withForeignPtr`, which uses `keepAlive#`, adding a lot of code to peek a
-- Word from a pointer.
-- Whether Claude's assumption that that code acts as an optimization fence
-- is correct is inconsequential, what matters is that we can remove all that
-- code by using pinned `ByteArray`s, and that the extra Word boxing+unboxing
-- indeed goes away with that.
--
-- After I wrote this, I realized _maybe_ I could've moved `withForeignPtr`
-- higher up in the call stack and in a single location, then pass down the
-- `Ptr Word8` in a newtype instead of doing this. But it wasn't only late,
-- `PinnedByteArray` has the advantage that I can push it down even to user
-- facing methods without being concerned with everything happening inside
-- the context of `withForeignPtr` (though I don't think it would've been a
-- problem). Also, we only use pinned byte arrays for our receive buffer,
-- which has such a short life span (it gets decoded into user rows immediately)
-- that heap fragmentation doesn't sound too concerning.
module Hpgsql.PinnedByteArray
  ( PinnedByteArray (..),
    LazyPinnedByteArray,
    createPinnedByteArray,
    takePgMessageIdentAndLen,
    drop,
    fromStrict,
    toStrict,
    splitAt,
    length,
    take,
    lazyLength,
    null,
    emptyPBA,
    fromByteString,
    toByteString,
    toStrictN,

    -- * Binary (de)serializer
    ByteStringIdx (..),
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
    CoolWordDec (..),
    WordDecoding (..),
    unsafeToUtf8Text,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString (..))
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Internal as InternalBS
import Data.Int (Int16, Int32, Int64)
import Foreign (withForeignPtr)
import Foreign.C (CInt (..))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import GHC.Base (Addr#, ByteArray#, Char (..), IO (..), Int (..), MutableByteArray#, RealWorld, byteArrayContents#, compareByteArrays#, indexWord8ArrayAsChar#, indexWord8ArrayAsWord32#, mutableByteArrayContents#, newPinnedByteArray#, unIO, unsafeFreezeByteArray#, (+#))
import GHC.Exts (indexWord8Array#, indexWord8ArrayAsWord16#, indexWord8ArrayAsWord64#)
import GHC.Ptr (Ptr (..))
import GHC.Word (Word32 (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude hiding (drop, encodeFloat, length, null, splitAt, take)
#if WORDS_BIGENDIAN
import Data.Word (Word16, Word32, Word64)
#else
import Data.Word (Word16, Word64, byteSwap16, byteSwap64, Word8, byteSwap32)
#endif
import Data.Array.Byte (ByteArray (..))
import Data.Bits (Bits (unsafeShiftR))
import Data.Coerce (coerce)
import Data.Text.Internal (Text (..))
import Foreign (Storable (..), (.&.))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import GHC.Word (Word16 (..), Word64 (..), Word8 (..))

data PinnedByteArray = PinnedByteArray
  { start :: !Int,
    len :: !Int,
    array :: !ByteArray#
  }

instance Eq PinnedByteArray where
  PinnedByteArray (I# s1) l1@(I# len) arr1# == PinnedByteArray (I# s2) l2 arr2# =
    l1 == l2 && case compareByteArrays# arr1# s1 arr2# s2 len of
      0# -> True
      _ -> False

-- TODO: dlist for efficient snoc, because buffers can grow very large when fetching binaries/json/text blobs
data LazyPinnedByteArray = LazyPinnedByteArray !Int ![PinnedByteArray]

instance Semigroup LazyPinnedByteArray where
  LazyPinnedByteArray l1 pbs1 <> LazyPinnedByteArray l2 pbs2 = LazyPinnedByteArray (l1 + l2) (pbs1 ++ pbs2)

instance Monoid LazyPinnedByteArray where
  mempty = LazyPinnedByteArray 0 []

{-# NOINLINE emptyPBA #-}
emptyPBA :: PinnedByteArray
emptyPBA = unsafeDupablePerformIO $ createPinnedByteArray 0 (\_ -> pure 0)

-- TODO: write property-based tests for these functions. This is tricky to get right.

createPinnedByteArray :: Int -> (Addr# -> IO CInt) -> IO PinnedByteArray
createPinnedByteArray (I# size#) f = IO $ \s0 ->
  let !(# newRW, (mutArr# :: MutableByteArray# RealWorld) #) = newPinnedByteArray# size# s0
      !(# newRW', lenCopied #) = unIO (f (mutableByteArrayContents# mutArr#)) newRW
      !(# finalRW, frozenArr# #) = unsafeFreezeByteArray# mutArr# newRW'
   in (# finalRW, PinnedByteArray 0 (fromIntegral lenCopied) frozenArr# #)

fromByteString :: ByteString -> PinnedByteArray
fromByteString (BS fptr len) = unsafeDupablePerformIO $ createPinnedByteArray len $ \dst -> withForeignPtr fptr $ \src -> do
  copyBytes (Ptr dst) src len
  pure $ fromIntegral len

toByteString :: PinnedByteArray -> ByteString
toByteString (PinnedByteArray start len src) = unsafeDupablePerformIO $ BS.create len $ \dst ->
  copyBytes dst (Ptr (byteArrayContents# src) `plusPtr` start) len

{-# INLINE unsafeToUtf8Text #-}
-- Assuming the pinned byte array contains valid UTF8 text, creates
-- returns an instance of `Text` with the same contents (but does make a copy).
unsafeToUtf8Text :: ByteStringIdx -> Int -> PinnedByteArray -> Either String Text
unsafeToUtf8Text idx desiredLen pba@(PinnedByteArray _ actualLen _) = if actualLen < desiredLen then Left "Not enough bytes to convert to Text" else let !(PinnedByteArray start arrLen arr#) = toStrictN idx.idx desiredLen (fromStrict pba) in Right $ Text (ByteArray arr#) start arrLen

takePgMessageIdentAndLen :: LazyPinnedByteArray -> Maybe (Char, Int32)
takePgMessageIdentAndLen lpba@(LazyPinnedByteArray len _) =
  if len >= 5
    then
      let !(PinnedByteArray (I# start) _ arr#) = toStrictN 0 5 lpba
       in Just (C# (indexWord8ArrayAsChar# arr# start), fromIntegral $ fromBigEndian32 $ W32# (indexWord8ArrayAsWord32# arr# (start +# 1#)))
    else Nothing

-- | Drops the next `n` bytes.
drop :: Int -> PinnedByteArray -> PinnedByteArray
drop n (PinnedByteArray start len arr#) =
  if n >= len
    then emptyPBA
    else
      PinnedByteArray (start + n) (len - n) arr#

-- | Takes the first `n` bytes.
take :: Int -> PinnedByteArray -> PinnedByteArray
take n (PinnedByteArray start len arr#) =
  PinnedByteArray start (min n len) arr#

fromStrict :: PinnedByteArray -> LazyPinnedByteArray
fromStrict pba@(PinnedByteArray _ len _) = LazyPinnedByteArray len [pba]

-- | Copies chunks into a single contiguous 'PinnedByteArray'. Avoids the copy
-- when there's already just a single chunk.
toStrict :: LazyPinnedByteArray -> PinnedByteArray
toStrict (LazyPinnedByteArray _ [pba]) = pba
toStrict lpba@(LazyPinnedByteArray totalLen _) = toStrictN 0 totalLen lpba

-- | Creates strict PBA from a Lazy one, but just with the first @n@
-- bytes after the first `skip` (or less if they're not all there).
toStrictN :: Int -> Int -> LazyPinnedByteArray -> PinnedByteArray
toStrictN skip n' (LazyPinnedByteArray totalLen' chunks) =
  let n = min n' totalLen'
   in unsafeDupablePerformIO $ createPinnedByteArray n $ \dst -> do
        let go copied _ _ [] = pure copied
            go copied _ 0 _ = pure copied
            go offset skipLeft nLeft (PinnedByteArray start l arr# : rest) = do
              let toSkipSrc = min l skipLeft
                  toCopy = min nLeft (l - toSkipSrc)
              when (toCopy > 0 && toSkipSrc < l) $ copyBytes (Ptr dst `plusPtr` offset) (Ptr (byteArrayContents# arr#) `plusPtr` (start + toSkipSrc)) toCopy
              when (toCopy < 0) $ error "toCopy < 0 should be impossible"
              go (offset + toCopy) (skipLeft - toSkipSrc) (nLeft - toCopy) rest
        fromIntegral <$> go 0 skip n chunks

splitAt :: Int -> PinnedByteArray -> (PinnedByteArray, PinnedByteArray)
splitAt n pba = (take n pba, drop n pba)

length :: PinnedByteArray -> Int
length (PinnedByteArray _ len _) = len

null :: PinnedByteArray -> Bool
null = (== 0) . length

lazyLength :: LazyPinnedByteArray -> Int
lazyLength (LazyPinnedByteArray len _) = len

-- * Binary (de)serializer

-- A replacement for libraries like cereal or binary.
-- In our tests, this is ~6.0% faster than cereal, and it also
-- (or by virtue of) allocates ~13% less memory in some of our benchmarks.
-- And it also means one fewer dependency.
-- The caveat is that this module makes unaligned memory access. For the target
-- CPU architectures of this library, this should be fine.

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
decodeWord :: CoolWordDec a -> ByteStringIdx -> PinnedByteArray -> (a -> a) -> Either String a
decodeWord wdec (ByteStringIdx boxedIdx@(I# idx)) (PinnedByteArray (I# start) len byArrSharp) endianConvert =
  case wdec of
    CWord8 -> if len < 1 + boxedIdx then Left "Less than enough bytes to decode" else Right $ endianConvert $ W8# $ indexWord8Array# byArrSharp (idx +# start)
    CWord16 -> if len < 2 + boxedIdx then Left "Less than enough bytes to decode" else Right $ endianConvert $ W16# $ indexWord8ArrayAsWord16# byArrSharp (idx +# start)
    CWord32 -> if len < 4 + boxedIdx then Left "Less than enough bytes to decode" else Right $ endianConvert $ W32# $ indexWord8ArrayAsWord32# byArrSharp (idx +# start)
    CWord64 -> if len < 8 + boxedIdx then Left "Less than enough bytes to decode" else Right $ endianConvert $ W64# $ indexWord8ArrayAsWord64# byArrSharp (idx +# start)

{-# INLINE unsafeEncodeWord #-}
unsafeEncodeWord :: (Storable a) => a -> (a -> a) -> Int -> ByteString
unsafeEncodeWord n endianConvert len =
  InternalBS.unsafeCreate len $ \bufferPtr ->
    poke (coerce bufferPtr) $ endianConvert n

newtype ByteStringIdx = ByteStringIdx {idx :: Int}
  deriving newtype (Num)

{-# INLINE decodeInt16BE #-}
decodeInt16BE :: ByteStringIdx -> PinnedByteArray -> Either String Int16
decodeInt16BE idx bs = fromIntegral <$> decodeWord CWord16 idx bs fromBigEndian16

{-# INLINE encodeInt16BE #-}
encodeInt16BE :: Int16 -> ByteString
encodeInt16BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian16 2

{-# INLINE decodeWord8 #-}
decodeWord8 :: ByteStringIdx -> PinnedByteArray -> Either String Word8
decodeWord8 idx bs = decodeWord CWord8 idx bs Prelude.id

{-# INLINE decodeWord32BE #-}
decodeWord32BE :: ByteStringIdx -> PinnedByteArray -> Either String Word32
decodeWord32BE idx bs = decodeWord CWord32 idx bs fromBigEndian32

{-# INLINE decodeWord64BE #-}
decodeWord64BE :: ByteStringIdx -> PinnedByteArray -> Either String Word64
decodeWord64BE idx bs = decodeWord CWord64 idx bs fromBigEndian64

{-# INLINE decodeInt32BE #-}
decodeInt32BE :: ByteStringIdx -> PinnedByteArray -> Either String Int32
decodeInt32BE idx bs = fromIntegral <$> decodeWord CWord32 idx bs fromBigEndian32

{-# INLINE encodeInt32BE #-}
encodeInt32BE :: Int32 -> ByteString
encodeInt32BE n = unsafeEncodeWord (fromIntegral n) fromBigEndian32 4

{-# INLINE decodeInt64BE #-}
decodeInt64BE :: ByteStringIdx -> PinnedByteArray -> Either String Int64
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
decodeDataRow :: ByteStringIdx -> PinnedByteArray -> Either String ByteStringIdx
decodeDataRow idx sbs@(PinnedByteArray _ len _) =
  -- We have a fast path when rows are at least 8 bytes long (should be the case
  -- for all but 0-column query results or bytestring chunks "cut in the middle of the message")
  -- by playing with bitwise operations.
  -- Whether this is worth keeping is sort of questionable. It's complex
  -- (even if I think it's safe and well tested) and reduces runtime of one of
  -- our benchmarks by 2% compared to not having it.
  case decodeWord CWord64 idx sbs fromBigEndian64 of
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
          msgIdentChar <- decodeWord8 idx sbs
          lenFullMsg <- decodeInt32BE (1 + idx) sbs
          if msgIdentChar == 68 -- Letter 'D'
            then toResult (fromIntegral lenFullMsg)
            else Left "Not a DataRow"
        else Left "Less than enough bytes to decode a DataRow"
  where
    toResult lenFullMsg
      | len >= 1 + lenFullMsg + idx.idx = Right $ ByteStringIdx $ 1 + lenFullMsg + idx.idx
      | otherwise = Left "Less than enough bytes to decode a full DataRow"

data WordDecoding a where
  TypeSize1 :: WordDecoding Word8
  TypeSize2 :: WordDecoding Word16
  TypeSize4 :: WordDecoding Word32

fromWordDec :: WordDecoding a -> CoolWordDec a
fromWordDec = \case
  TypeSize1 -> CWord8
  TypeSize2 -> CWord16
  TypeSize4 -> CWord32

{-# INLINE decodePgFieldWithAtMost4Bytes #-}

-- | A specialized decoder that decoders a query result's
-- field's contents, but only for PG fields at most 4 bytes long and
-- at least 1 byte long (so no text or void types, for example).
-- This includes essentially int32, int16, and booleans.
-- Pass in as type argument a Word8, Word16 or Word32 to indicate
-- the size of the PG type you're decoding.
-- Returns the index into the first yet-unparsed byte.
decodePgFieldWithAtMost4Bytes :: forall a. (Storable a, Integral a) => WordDecoding a -> ByteStringIdx -> PinnedByteArray -> Either String (Maybe a, ByteStringIdx)
decodePgFieldWithAtMost4Bytes wdec =
  let (pgTypeSize, endianSwap, valueMask :: Word64) = case wdec of
        TypeSize1 -> (1, Prelude.id, 0b00000000_00000000_00000000_00000000_11111111_00000000_00000000_00000000)
        TypeSize2 -> (2, fromBigEndian16, 0b00000000_00000000_00000000_00000000_11111111_11111111_00000000_00000000)
        TypeSize4 -> (4, fromBigEndian32, 0b00000000_00000000_00000000_00000000_11111111_11111111_11111111_11111111)
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
                fieldValue <- decodeWord (fromWordDec wdec) (idx + 4) bs endianSwap
                Right (Just fieldValue, idx + 4 + fromIntegral lenField)
              else Right (Nothing, idx + 4)
