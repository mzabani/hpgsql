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
import Data.Monoid (Sum (..))
import Data.Word (Word8)

data BinaryField = SqlNull | NotNull !ByteString
  deriving stock (Eq)

instance Show BinaryField where
  show SqlNull = "NULL"
  show (NotNull bs) = show bs

data LengthAwareBuilder = LengthAwareBuilder !(Sum Int32) !Builder.Builder

type Builder = LengthAwareBuilder

instance Semigroup LengthAwareBuilder where
  LengthAwareBuilder l1 b1 <> LengthAwareBuilder l2 b2 = LengthAwareBuilder (l1 <> l2) (b1 <> b2)

instance Monoid LengthAwareBuilder where
  mempty = LengthAwareBuilder (Sum 0) mempty

builderLength :: LengthAwareBuilder -> Int32
builderLength (LengthAwareBuilder (Sum n) _) = n

binaryField :: BinaryField -> LengthAwareBuilder
binaryField = \case
  SqlNull -> LengthAwareBuilder (Sum 4) (Builder.int32BE (-1))
  NotNull val -> let valLen = fromIntegral (BS.length val) in LengthAwareBuilder (Sum $ 4 + valLen) (Builder.int32BE valLen <> Builder.byteString val)

toLazyByteString :: LengthAwareBuilder -> LBS.ByteString
toLazyByteString (LengthAwareBuilder (Sum (fromIntegral -> totalLen)) builder) = Builder.toLazyByteStringWith (Builder.untrimmedStrategy totalLen 0) mempty builder

toStrictByteString :: LengthAwareBuilder -> ByteString
toStrictByteString = LBS.toStrict . toLazyByteString

char7 :: Char -> LengthAwareBuilder
char7 c = LengthAwareBuilder (Sum 1) (Builder.char7 c)

string7 :: String -> LengthAwareBuilder
string7 s = LengthAwareBuilder (Sum $ fromIntegral $ length s) (Builder.string7 s)

word8 :: Word8 -> LengthAwareBuilder
word8 s = LengthAwareBuilder (Sum 1) (Builder.word8 s)

int16BE :: Int16 -> LengthAwareBuilder
int16BE n = LengthAwareBuilder (Sum 2) (Builder.int16BE n)

int32BE :: Int32 -> LengthAwareBuilder
int32BE n = LengthAwareBuilder (Sum 4) (Builder.int32BE n)

int64BE :: Int64 -> LengthAwareBuilder
int64BE n = LengthAwareBuilder (Sum 8) (Builder.int64BE n)

byteString :: ByteString -> LengthAwareBuilder
byteString bs = LengthAwareBuilder (Sum $ fromIntegral $ BS.length bs) (Builder.byteString bs)

lazyByteString :: LBS.ByteString -> LengthAwareBuilder
lazyByteString bs = LengthAwareBuilder (Sum $ fromIntegral $ LBS.length bs) (Builder.lazyByteString bs)
