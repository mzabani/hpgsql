module DbUtils where

import Control.Monad
  ( forM_,
    void,
  )
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import HPgsql (ConnString (..), ConnectOpts (..), HPgConnection, defaultConnectOpts, execute, execute_, withConnection, withConnectionOpts)
import System.Environment (getEnv)
import System.Mem (performGC)
import Test.Hspec

testConnInfo :: IO ConnString
testConnInfo = getEnv "PGPORT" >>= \portStr -> pure ConnString {user = "postgres", database = "postgres", hostname = "127.0.0.1", port = read portStr, password = "", options = ""}

aroundConn :: SpecWith HPgConnection -> Spec
aroundConn = around $ \act -> do
  connstr <- testConnInfo
  -- Use use a very low polling interval to hopefully exercise code paths
  -- where polling does not detect dead threads in the very first check
  withConnectionOpts defaultConnectOpts {killedThreadPollIntervalMs = 1} connstr 10 act
