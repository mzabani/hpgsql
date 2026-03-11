module TestUtils (genJsonValue) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genJsonValue :: Gen Aeson.Value
genJsonValue =
  Gen.recursive
    Gen.choice
    -- Non-recursive cases
    [ pure Aeson.Null,
      Aeson.Bool <$> Gen.bool,
      Aeson.Number . fromIntegral <$> Gen.int (Range.linearFrom 0 (-1000000) 1000000),
      Aeson.Number . realToFrac <$> Gen.double (Range.linearFracFrom 0 (-1e6) 1e6),
      Aeson.String <$> Gen.text (Range.linear 0 20) Gen.unicode
    ]
    -- Recursive cases
    [ Aeson.Array . Vector.fromList <$> Gen.list (Range.linear 0 5) genJsonValue,
      Aeson.Object . Aeson.KeyMap.fromList <$> Gen.list (Range.linear 0 5) genKeyValue
    ]
  where
    genKeyValue = (,) <$> (Aeson.Key.fromText <$> Gen.text (Range.linear 1 10) Gen.alphaNum) <*> genJsonValue
