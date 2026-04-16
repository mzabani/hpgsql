{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ConnectivitySpec where

import Data.Text (Text)
import DbUtils
  ( irrecoverableErrorMustContain,
    testConnInfo,
  )
import Hpgsql
import Hpgsql.Query (sql)
import Test.Hspec

spec :: Spec
spec = do
  describe "Connectivity" $ parallel $ do
    it
      "Connecting to non-existing db"
      connectingToNonExistingDb
    it
      "Connecting with connect-time options"
      connectingWithConnectTimeOptions

connectingToNonExistingDb :: IO ()
connectingToNonExistingDb = do
  hpgsqlConnInfo <- testConnInfo
  connect hpgsqlConnInfo {database = "non-existing-db"} 10 `shouldThrow` irrecoverableErrorMustContain [(ErrorCode, "3D000"), (ErrorHumanReadableMsg, "database \"non-existing-db\" does not exist")]

connectingWithConnectTimeOptions :: IO ()
connectingWithConnectTimeOptions = do
  hpgsqlConnInfo <- testConnInfo
  withConnection hpgsqlConnInfo {options = "-c my.random_setting=4"} 10 $ \conn -> do
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
    execute_ conn "SET my.random_setting=7"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("7" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
