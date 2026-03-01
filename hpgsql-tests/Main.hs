module Main where

import qualified AllSpecs
import GHC.RTS.Flags (RTSFlags (..), getRTSFlags)
import qualified System.IO as IO
import Test.Hspec

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.NoBuffering

  getRTSFlags >>= \f -> print (parFlags f)

  hspec AllSpecs.spec
