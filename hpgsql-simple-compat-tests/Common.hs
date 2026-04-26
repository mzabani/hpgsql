module Common
  ( module Database.PostgreSQL.Simple,
    module Test.Tasty.HUnit,
    TestEnv (..),
  )
where

import Database.PostgreSQL.Simple
import Test.Tasty.HUnit

data TestEnv
  = TestEnv
  { -- | Connection shared by all the tests
    conn :: Connection,
    -- | Bracket for spawning additional connections
    withConn :: forall a. (Connection -> IO a) -> IO a
  }
