module Common
  ( module Database.PostgreSQL.Simple,
    module Test.Tasty.HUnit,
    TestEnv (..),
    -- md5,
  )
where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Test.Tasty.HUnit

data TestEnv
  = TestEnv
  { -- | Connection shared by all the tests
    conn :: Connection,
    -- | Bracket for spawning additional connections
    withConn :: forall a. (Connection -> IO a) -> IO a
  }

-- | Return the MD5 hash of a 'ByteString', in lowercase hex format.
--
-- Example:
--
-- >[Only hash] <- query_ conn "SELECT md5('hi')"
-- >assertEqual "md5('hi')" (md5 "hi") hash
-- md5 :: ByteString -> Text
-- md5 = TE.decodeUtf8 . Base16.encode . MD5.hash
