module PinnedByteArraySpec where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Gen, PropertyT, annotateShow, (===))
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Gen
import Hpgsql.PinnedByteArray.Internal (PinnedByteArray)
import qualified Hpgsql.PinnedByteArray.Internal as PBA
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import TestUtils (genJsonValue)

spec :: Spec
spec = parallel $ do
  describe "PinnedByteArray" $ do
    it
      "slicing strict and lazy PBAs"
      slicingStrictAndLazyPBAs
    it
      "unsafeToUtf8Text identity"
      unsafeToUtf8TextIdentity

-- | Indirectly this tests pretty much every function
-- in the PinnedByteArray module, including lazy concatenation,
-- to/fromByteString, strict and lazy lengths, take, drop, and
-- copyStrictSlice.
-- It uses ByteString's implementation for comparison.
slicingStrictAndLazyPBAs :: PropertyT IO ()
slicingStrictAndLazyPBAs = hedgehog $ do
  bss :: [ByteString] <- Gen.forAll $ Gen.list (Gen.linear 0 10) (Gen.bytes (Gen.linear 0 100))
  let lazyBs = LBS.fromChunks bss
      lazyPba = foldl' (\acc bs -> acc <> PBA.fromStrict (PBA.fromByteString bs)) mempty bss
      bs = LBS.toStrict lazyBs
      pba = PBA.fromByteString bs
      len = PBA.length pba

  -- `toStrict` here will just match on a singleton list.. but
  -- let's test it anyway
  PBA.toStrict lazyPba === pba
  len === PBA.lazyLength lazyPba

  -- Now we test with varied offset and length because these
  -- functions have a lot of bounds-checking in them, which is
  -- hard to get right.
  skip <- Gen.forAll $ Gen.int (Gen.linear 0 (len + 10))
  n <- Gen.forAll $ Gen.int (Gen.linear 0 (len + 10))
  let slicedBs = BS.take n (BS.drop skip bs)
      slicedPba = PBA.take n (PBA.drop skip pba)
      slicedLazyPba = PBA.copyStrictSlice skip n lazyPba
  PBA.toByteString slicedPba === slicedBs
  slicedPba === PBA.fromByteString slicedBs
  PBA.length slicedPba === BS.length slicedBs
  PBA.toByteString slicedLazyPba === slicedBs
  PBA.length slicedLazyPba === BS.length slicedBs

unsafeToUtf8TextIdentity :: PropertyT IO ()
unsafeToUtf8TextIdentity = hedgehog $ do
  t :: Text <- Gen.forAll $ Gen.text (Gen.linear 0 9999) Gen.unicode
  let bs = encodeUtf8 t
  let pba = PBA.fromByteString bs
  PBA.unsafeToUtf8Text 0 (PBA.length pba) pba === Right t
  -- Out-of-bounds correctly detected
  PBA.unsafeToUtf8Text 0 (PBA.length pba + 1) pba === Left "Insufficient bytes in buffer in unsafeToUtf8Text"

genPba :: Gen PinnedByteArray
genPba = PBA.fromByteString <$> Gen.bytes (Gen.linear 0 9999)
