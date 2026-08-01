module LibPqConnStringParsingSpec
  ( spec,
  )
where

import Control.Monad (forM_)
import qualified Data.Char as Char
import Data.Hashable (Hashable, hash)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word16)
import Hedgehog (Gen, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hpgsql.Connection (ConnectionString (..), parseLibpqConnectionString, renderLibpqConnectionString)
import Network.URI
  ( URI (..),
    URIAuth (..),
    escapeURIString,
    isUnescapedInURIComponent,
    uriToString,
  )
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

-- This test suite was copied over from my other project, codd, and changed to use
-- hedgehog instead of quickcheck, plus a few tests removed because they didn't make sense here.
-- It would be nice to produce "options" fields for the connection strings too, these tests still
-- produce them as empty.

spec :: Spec
spec = do
  describe "Connection string parsing" $ do
    it "Some hard coded connection strings" $ do
      -- These are hard coded connection strings that I've tested with psql at some point
      let stringsAndExpectedConnInfos =
            [ ( "postgresql://postgres@localhost/some%20thing%20%2F%3F%3A%40",
                ConnectionString
                  { hostname = "localhost",
                    port = 5432,
                    user = "postgres",
                    password = "",
                    database = "some thing /?:@",
                    options = ""
                  }
              ),
              ( "postgresql://some%20thing%20%2F%3F%3A%40:passwdsome%20thing%20%2F%3F%3A%40@localhost:1/some%20thing%20%2F%3F%3A%40",
                ConnectionString
                  { hostname = "localhost",
                    port = 1,
                    user = "some thing /?:@",
                    password = "passwdsome thing /?:@",
                    database = "some thing /?:@",
                    options = ""
                  }
              ),
              -- Only mandatory arguments
              ( "postgresql://postgres@[::1]/somedb",
                ConnectionString
                  { hostname = "::1",
                    port = 5432,
                    user = "postgres",
                    password = "",
                    database = "somedb",
                    options = ""
                  }
              ),
              ( "dbname='some thing /?:@' user='some thing /?:@'   host = localhost   ",
                ConnectionString
                  { hostname = "localhost",
                    port = 5432,
                    user = "some thing /?:@",
                    password = "",
                    database = "some thing /?:@",
                    options = ""
                  }
              ),
              ( "dbname='some thing /?:@'\nuser='some thing /?:@' password='passwdsome thing /?:@' port=1 host = localhost\n",
                ConnectionString
                  { hostname = "localhost",
                    port = 1,
                    user = "some thing /?:@",
                    password = "passwdsome thing /?:@",
                    database = "some thing /?:@",
                    options = ""
                  }
              ),
              ( "\ndbname=codd-experiments   \t\n    user=postgres port=5433 host=::1\n",
                ConnectionString
                  { hostname = "::1",
                    port = 5433,
                    user = "postgres",
                    password = "",
                    database = "codd-experiments",
                    options = ""
                  }
              ),
              -- Only mandatory arguments
              ( "dbname=codd-experiments\tuser=postgres\rhost=127.0.0.1",
                ConnectionString
                  { hostname = "127.0.0.1",
                    port = 5432,
                    user = "postgres",
                    password = "",
                    database = "codd-experiments",
                    options = ""
                  }
              ),
              -- I didn't really test this with psql, but IIUC = signs should be accepted as parts of values
              ( "dbname=codd-experiments\t  user=  post=gres password='abc = def'  host=127.0.0.1",
                ConnectionString
                  { hostname = "127.0.0.1",
                    port = 5432,
                    user = "post=gres",
                    password = "abc = def",
                    database = "codd-experiments",
                    options = ""
                  }
              )
            ]
      forM_ stringsAndExpectedConnInfos $ \(connString, expectedConnInfo) -> do
        parseLibpqConnectionString
          connString
          `shouldBe` Right expectedConnInfo

    it "Randomized valid connection strings" $ hedgehog $ do
      connGen <- forAll genConnStringGen
      let expectedConn = connInfoFromConnStringGen connGen
      parseLibpqConnectionString (renderConnStringGen connGen) === Right expectedConn
      -- Round-tripping with `renderLibpqConnectionString`
      parseLibpqConnectionString (decodeUtf8 $ renderLibpqConnectionString expectedConn) === Right expectedConn

newtype ObjName = ObjName {unObjName :: Text}
  deriving newtype (Eq, Ord, Show, Hashable)

validLowerFirstChars :: [Char]
validLowerFirstChars = '_' : ['a' .. 'z']

validUpperFirstChars :: [Char]
validUpperFirstChars = ['A' .. 'Z']

genObjName :: Gen ObjName
genObjName =
  ObjName . Text.pack
    <$> Gen.frequency
      [(100, genLower), (5, genMixed)]
  where
    validLowerOtherChars = validLowerFirstChars ++ "0123456789$"
    validUpperOtherChars = validUpperFirstChars ++ "0123456789$"

    genLower = do
      c <- Gen.element validLowerFirstChars
      -- Max Length 63 bytes of UTF8-Encoded name
      r <- Gen.list (Range.linear 0 62) $ Gen.element validLowerOtherChars
      pure $ c : r
    genMixed = do
      c <- Gen.element $ validLowerFirstChars ++ validUpperFirstChars
      -- Max Length 63 bytes of UTF8-Encoded name
      r <-
        Gen.list (Range.linear 0 62) $
          Gen.element $
            validLowerOtherChars
              ++ validUpperOtherChars
      pure $ c : r

data HostString = Hostname Text | HostUnixSocket Text
  deriving stock (Show)

originalHost :: HostString -> Text
originalHost (Hostname t) = t
originalHost (HostUnixSocket t) = t

genHostString :: Gen HostString
genHostString =
  Gen.element
    [ Hostname "127.0.0.1",
      Hostname "200.200.100.100",
      Hostname "hostnodots",
      Hostname "some.host.name",
      Hostname "::1",
      Hostname "2800:3f0:4001:822::200e",
      Hostname "2a03:2880:f105:283:face:b00c:0:25de",
      HostUnixSocket "/var/lib/postgresql"
    ]

data ConnStringType = URIpostgres | Kwvps
  deriving stock (Bounded, Enum, Show)

getURIConnString ::
  Text -> ObjName -> Text -> HostString -> Word16 -> ObjName -> Text
getURIConnString (Text.unpack -> uriScheme) (Text.unpack . unObjName -> usr) (Text.unpack -> pwd) host port (Text.unpack . unObjName -> dbName) =
  Text.pack $ uriToString id uri ""
  where
    uri =
      URI
        { uriScheme,
          uriAuthority =
            Just
              URIAuth
                { uriUserInfo =
                    encodeURIComponent usr
                      <> ( if pwd == ""
                             then ""
                             else ':' : encodeURIComponent pwd
                         )
                      <> "@",
                  uriRegName = Text.unpack $ escapeHost host,
                  uriPort = ':' : show port
                },
          uriPath = '/' : encodeURIComponent dbName,
          uriQuery = "",
          uriFragment = ""
        }

encodeURIComponent :: String -> String
encodeURIComponent = escapeURIString isUnescapedInURIComponent

data KwvpConnGen = KwvpConnGen
  { -- | Shuffles the relative order of keyword/value pairs in the generated connection string.
    shuffleIdx :: Int,
    -- | The boolean dictates quoting when quoting is optional.
    -- The Int represents extra spaces around this kwvp. These will be added one by one
    -- left-to-right everywhere they are acceptable until a limit of 4 (therefore ignoring > 4 spaces).
    user :: (ObjName, Bool, Int),
    kwvp_password :: Maybe (Text, Bool, Int),
    host :: (HostString, Bool, Int),
    kwvp_port :: Maybe (Word16, Bool, Int),
    dbname :: (ObjName, Bool, Int)
  }
  deriving stock (Show)

getKeywordValuePairConnString :: KwvpConnGen -> Text
getKeywordValuePairConnString KwvpConnGen {..} =
  Text.intercalate " " $
    map (\(kw, v) -> kw <> "=" <> v) mixedKwvps
  where
    mixedKwvps =
      sortOn ((`mod` max 1 shuffleIdx) . hash) $
        catMaybes
          [ Just $ addExtraSpaces user ("user", quoteIfNeeded unObjName user),
            Just $
              addExtraSpaces
                host
                ("host", quoteIfNeeded originalHost host),
            Just $
              addExtraSpaces dbname ("dbname", quoteIfNeeded unObjName dbname),
            (\p -> addExtraSpaces p ("password", quoteIfNeeded id p))
              <$> kwvp_password,
            (\p -> addExtraSpaces p ("port", quoteIfNeeded (Text.pack . show) p))
              <$> kwvp_port
          ]
    addExtraSpaces (_, _, spaces) (k, v)
      | spaces <= 0 = (k, v)
      | spaces == 1 = (" " <> k, v)
      | spaces == 2 = (" " <> k <> " ", v)
      | spaces == 3 = (" " <> k <> " ", " " <> v)
      | otherwise = (" " <> k <> " ", " " <> v <> " ")
    quoteIfNeeded un (v, force, _) =
      if needsQuoting (un v) || force
        then
          "'"
            <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" (un v))
            <> "'"
        else un v
    needsQuoting v =
      "'"
        `Text.isInfixOf` v
        || "\\"
          `Text.isInfixOf` v
        || Text.any Char.isSpace v
        || v
          == ""

escapeHost :: HostString -> Text
escapeHost (Hostname host) =
  -- bracket-escape IPv6 addresses
  if ":" `Text.isInfixOf` host then "[" <> host <> "]" else host
escapeHost (HostUnixSocket socketpath) = Text.pack $ encodeURIComponent $ Text.unpack socketpath

newtype ConnStringGen = ConnStringGen (Either KwvpConnGen (Text, ObjName, Text, HostString, Word16, ObjName))

instance Show ConnStringGen where
  show (ConnStringGen kwvpOrUriScheme) = Text.unpack $
    case kwvpOrUriScheme of
      Left kwvpConnGen -> getKeywordValuePairConnString kwvpConnGen
      Right (uriScheme, usr, pwd, host, port, dbName) ->
        getURIConnString uriScheme usr pwd host port dbName

renderConnStringGen :: ConnStringGen -> Text
renderConnStringGen (ConnStringGen kwvpOrUriScheme) =
  case kwvpOrUriScheme of
    Left kwvpConnGen -> getKeywordValuePairConnString kwvpConnGen
    Right (uriScheme, usr, pwd, host, port, dbName) ->
      getURIConnString uriScheme usr pwd host port dbName

connInfoFromConnStringGen :: ConnStringGen -> ConnectionString
connInfoFromConnStringGen (ConnStringGen kwvpOrUriScheme) =
  case kwvpOrUriScheme of
    Left KwvpConnGen {user = (user, _, _), host = (host, _, _), dbname = (dbname, _, _), kwvp_password, kwvp_port} ->
      ConnectionString
        { hostname = originalHost host,
          port = case kwvp_port of
            Nothing -> 5432
            Just (p, _, _) -> p,
          user = unObjName user,
          password = case kwvp_password of
            Nothing -> ""
            Just (p, _, _) -> p,
          database = unObjName dbname,
          options = ""
        }
    Right (_uriScheme, user, password, host, port, dbname) ->
      ConnectionString
        { hostname = originalHost host,
          port = port,
          user = unObjName user,
          password = password,
          database = unObjName dbname,
          options = ""
        }

genConnStringGen :: Gen ConnStringGen
genConnStringGen = do
  usr <- genObjName
  pwd <-
    Gen.choice
      [ pure "",
        Gen.list (Range.linear 0 100) (Gen.element [' ' .. '~'])
      ]
  host <- genHostString
  port <- Gen.choice [pure 5432, Gen.word16 Range.linearBounded]
  dbName <- genObjName

  let connInfo =
        ConnectionString
          { hostname = originalHost host,
            port,
            user = unObjName usr,
            password = Text.pack pwd,
            database = unObjName dbName,
            options = ""
          }
  connStringType <- Gen.enumBounded @_ @ConnStringType
  case connStringType of
    Kwvps -> do
      shuffleIdx <- Gen.int Range.linearBounded
      b1 <- Gen.bool
      s1 <- Gen.int Range.linearBounded
      b2 <- Gen.bool
      s2 <- Gen.int Range.linearBounded
      b3 <- Gen.bool
      s3 <- Gen.int Range.linearBounded
      b4 <- Gen.bool
      s4 <- Gen.int Range.linearBounded
      b5 <- Gen.bool
      s5 <- Gen.int Range.linearBounded
      withPwd <-
        if connInfo.password == ""
          then Gen.bool
          else pure True
      withPort <-
        if port == 5432
          then Gen.bool
          else pure True
      pure $
        ConnStringGen $
          Left $
            KwvpConnGen
              { shuffleIdx,
                user = (usr, b1, s1),
                kwvp_password =
                  if withPwd
                    then Just (Text.pack pwd, b2, s2)
                    else Nothing,
                host = (host, b3, s3),
                kwvp_port =
                  if withPort
                    then Just (port, b4, s4)
                    else Nothing,
                dbname = (dbName, b5, s5)
              }
    URIpostgres -> do
      uriScheme <- Gen.element ["postgres:", "postgresql:"]
      pure $
        ConnStringGen $
          Right (uriScheme, usr, Text.pack pwd, host, port, dbName)
