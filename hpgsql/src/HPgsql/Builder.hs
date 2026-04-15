module HPgsql.Builder where

-- \| This module replicates parts of the API of Data.ByteString.Builder but its own
-- builder is length-aware, which makes other parts of the code a little bit nicer.
-- In COPY benchmarks, this module was introduced in a commit (together with other
-- changes, like replacing `Maybe` with `BinaryField` in `ToPgField`) that barely
-- changed memory usage and runtime.
-- The benefits are exclusively for code readability, then.
-- \|

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8)

data BinaryField = SqlNull | NotNull !ByteString
  deriving stock (Eq)

instance Show BinaryField where
  show SqlNull = "NULL"
  show (NotNull bs) = show bs

data LengthAwareBuilder = LengthAwareBuilder !Int32 !Builder.Builder

type Builder = LengthAwareBuilder

instance Semigroup LengthAwareBuilder where
  LengthAwareBuilder l1 b1 <> LengthAwareBuilder l2 b2 = LengthAwareBuilder (l1 + l2) (b1 <> b2)

instance Monoid LengthAwareBuilder where
  mempty = LengthAwareBuilder 0 mempty

builderLength :: LengthAwareBuilder -> Int32
builderLength (LengthAwareBuilder n _) = n

binaryField :: BinaryField -> LengthAwareBuilder
binaryField = \case
  SqlNull -> LengthAwareBuilder 4 (Builder.int32BE (-1))
  NotNull val -> let valLen = fromIntegral (BS.length val) in LengthAwareBuilder (4 + valLen) (Builder.int32BE valLen <> Builder.byteString val)

toLazyByteString :: LengthAwareBuilder -> LBS.ByteString
toLazyByteString (LengthAwareBuilder (fromIntegral -> totalLen) builder) = Builder.toLazyByteStringWith (Builder.untrimmedStrategy totalLen 0) mempty builder

toStrictByteString :: LengthAwareBuilder -> ByteString
toStrictByteString = LBS.toStrict . toLazyByteString

char7 :: Char -> LengthAwareBuilder
char7 c = LengthAwareBuilder 1 (Builder.char7 c)

string7 :: String -> LengthAwareBuilder
string7 s = LengthAwareBuilder (fromIntegral $ length s) (Builder.string7 s)

word8 :: Word8 -> LengthAwareBuilder
word8 s = LengthAwareBuilder 1 (Builder.word8 s)

int16BE :: Int16 -> LengthAwareBuilder
int16BE n = LengthAwareBuilder 2 (Builder.int16BE n)

int32BE :: Int32 -> LengthAwareBuilder
int32BE n = LengthAwareBuilder 4 (Builder.int32BE n)

int64BE :: Int64 -> LengthAwareBuilder
int64BE n = LengthAwareBuilder 8 (Builder.int64BE n)

byteString :: ByteString -> LengthAwareBuilder
byteString bs = LengthAwareBuilder (fromIntegral $ BS.length bs) (Builder.byteString bs)

lazyByteString :: LBS.ByteString -> LengthAwareBuilder
lazyByteString bs = LengthAwareBuilder (fromIntegral $ LBS.length bs) (Builder.lazyByteString bs)
