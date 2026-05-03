module ConnectivitySpec where

import qualified Data.List as List
import Data.Text (Text)
import DbUtils
  ( irrecoverableErrorMustContain,
    irrecoverableErrorWithMsg,
    testConnInfo,
  )
import Hpgsql
import Hpgsql.Cancellation (cancelAnyRunningStatement)
import Hpgsql.Query (sql)
import Hpgsql.Types (Only (..))
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
    it
      "Connecting with unencrypted password"
      connectingWithUnencryptedPassword

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

connectingWithUnencryptedPassword :: IO ()
connectingWithUnencryptedPassword = do
  hpgsqlConnInfo <- testConnInfo
  connect hpgsqlConnInfo {user = "user_pass", password = "WRONG-password"} 10 `shouldThrow` \(ex :: IrrecoverableHpgsqlError) -> "password authentication failed for user" `List.isInfixOf` show ex
  Only connectedUser <- withConnection hpgsqlConnInfo {user = "user_pass", password = "hpgsql-password"} 10 $ \conn -> do
    cancelAnyRunningStatement conn False -- This requires connecting again, so it's a reasonable test
    query1 conn "SELECT current_user"
  connectedUser `shouldBe` ("user_pass" :: String)
