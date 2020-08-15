module HPgsql.Connection
  ( ConnString (..),
    parseConnString,
    libpqConnString,
  )
where

import Control.Applicative
  ( (<|>),
  )
import Control.Monad
  ( unless,
    void,
    when,
  )
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfInput,
    parseOnly,
    peekChar,
    skipWhile,
    takeWhile1,
  )
import qualified Data.Attoparsec.Text as Parsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Functor.Identity (Identity (..))
import Data.List
  ( sortOn,
  )
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16)
import Network.URI
  ( URI (..),
    URIAuth (..),
    parseURI,
    unEscapeString,
  )
import Prelude hiding (takeWhile)

-- TODO: Switch to Text as the default in record fields
data ConnString = ConnString
  { hostname :: String,
    port :: Word16,
    user :: String,
    password :: String,
    database :: String,
    options :: String
  }
  deriving stock (Eq)

-- | Renders a libpq compatible connection string.
-- TODO: Copy tests from codd!
libpqConnString :: ConnString -> ByteString
libpqConnString ConnString {..} =
  ByteString.intercalate " " $
    map (\(kw, v) -> kw <> "=" <> v) mixedKwvps
  where
    mixedKwvps =
      catMaybes
        [ Just ("user", quote user),
          Just
            ("host", quote hostname),
          Just
            ("dbname", quote database),
          Just ("password", quote password),
          Just ("port", quote (show port)),
          if null options then Nothing else Just ("options", quote options)
        ]
    quote (Text.pack -> un) =
      encodeUtf8 $
        "'"
          <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" un)
          <> "'"

instance Show ConnString where
  show _ = "ConnectionString"

parseConnString :: Text -> Either String ConnString
parseConnString cstr =
  parseOnly (connStringParser <* endOfInput) cstr

-- | Parser that consumes any kind of Unicode space character, including \t, \n, \r, \f, \v.
skipAllWhiteSpace :: Parser ()
skipAllWhiteSpace = skipWhile Char.isSpace

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Stops at and does not consume the first predicate-passing
-- char, and does not include escape chars in the returned value,
-- as one would expect.
parseWithEscapeCharProper :: (Char -> Bool) -> Parser Text
parseWithEscapeCharProper untilc = do
  cs <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
  nextChar <- peekChar
  case nextChar of
    Nothing -> pure cs
    Just '\\' -> do
      void $ char '\\'
      c <- Parsec.take 1
      rest <- parseWithEscapeCharProper untilc
      pure $ cs <> c <> rest
    Just _ -> pure cs

eitherToMay :: Either a b -> Maybe b
eitherToMay (Left _) = Nothing
eitherToMay (Right v) = Just v

-- | Parses a URI with scheme 'postgres' or 'postgresql', as per https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING.
-- The difference here is that URIs with a query string or with a fragment are not allowed.
uriConnParser :: Text -> Either String ConnString
uriConnParser line = runIdentity $ runExceptT @String @_ @ConnString $ do
  case parseURI (Text.unpack line) of
    Nothing -> throwE "Connection string is not a URI"
    Just URI {..} -> do
      unless
        (Text.toLower (Text.pack uriScheme) `elem` ["postgres:", "postgresql:"])
        $ throwE
          "Connection string's URI scheme must be 'postgres' or 'postgresql'"
      case uriAuthority of
        Nothing ->
          throwE
            "Connection string must contain at least user and host"
        Just URIAuth {..} -> do
          let database =
                unEscapeString $ trimFirst '/' uriPath
              hasQueryString = not $ null uriQuery
              hasFragment = not $ null uriFragment
          when (null database) $
            throwE
              "Connection string must contain a database name"
          when (hasQueryString || hasFragment) $
            throwE
              "Custom parameters are not supported in connection strings. Make sure your connection URI does not have a query string or query fragment"

          -- Ports are not mandatory and are defaulted to 5432 when not present
          let port = if null uriPort then Just 5432 else Nothing
          case port
            <|> eitherToMay
              ( parseOnly
                  (Parsec.decimal <* endOfInput)
                  (Text.pack $ trimFirst ':' uriPort)
              ) of
            Nothing ->
              throwE "Invalid port in connection string"
            Just parsedPort -> do
              let (unEscapeString . trimLast '@' -> user, unEscapeString . trimLast '@' . trimFirst ':' -> password) =
                    break (== ':') uriUserInfo
              pure
                ConnString
                  { hostname =
                      unEscapeString $
                        unescapeIPv6 uriRegName,
                    port = parsedPort,
                    user,
                    password,
                    database,
                    options = ""
                  }
  where
    unescapeIPv6 :: String -> String
    unescapeIPv6 = trimFirst '[' . trimLast ']'

    trimFirst :: Char -> String -> String
    trimFirst c s@(c1 : cs) = if c == c1 then cs else s
    trimFirst _ s = s

    trimLast :: Char -> String -> String
    trimLast c s = case Text.unsnoc $ Text.pack s of
      Nothing -> s
      Just (t, lastChar) -> if lastChar == c then Text.unpack t else s

keywordValueConnParser :: Text -> Either String ConnString
keywordValueConnParser line = runIdentity $ runExceptT $ do
  kvs <-
    sortOn fst
      <$> parseOrFail
        (singleKeyVal `Parsec.sepBy` takeWhile1 Char.isSpace)
        (Text.strip line)
        "Invalid connection string"
  ConnString
    <$> getVal "host" Nothing txtToString kvs
    <*> getVal "port" (Just 5432) Parsec.decimal kvs
    <*> getVal "user" Nothing txtToString kvs
    <*> getVal "password" (Just "") txtToString kvs
    <*> getVal "dbname" Nothing txtToString kvs
    <*> pure ""
  where
    getVal key def parser pairs =
      case (map snd $ filter ((== key) . fst) pairs, def) of
        ([], Nothing) ->
          throwE $
            "Connection string must contain a value for '"
              <> Text.unpack key
              <> "'"
        ([], Just v) -> pure v
        ([vt], _) ->
          parseOrFail parser vt $
            "Connection string key '"
              <> Text.unpack key
              <> "' is in an unrecognizable format"
        _ ->
          throwE $
            "Duplicate key '"
              <> Text.unpack key
              <> "' found in connection string."

    txtToString = Text.unpack <$> Parsec.takeText
    parseOrFail parser txt errorMsg =
      case parseOnly (parser <* endOfInput) txt of
        Left _ -> throwE errorMsg
        Right v -> pure v

    singleKeyVal = do
      key <- takeWhile1 (\c -> not (Char.isSpace c) && c /= '=')
      skipAllWhiteSpace
      void $ char '='
      skipAllWhiteSpace
      value <-
        takeQuotedString
          <|> parseWithEscapeCharProper
            (\c -> Char.isSpace c || c == '\'' || c == '\\')
          <|> pure ""
      pure (key, value)

    takeQuotedString = do
      void $ char '\''
      s <- parseWithEscapeCharProper (== '\'')
      void $ char '\''
      pure s

-- | Parses a string in one of libpq allowed formats. See https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING.
-- The difference here is that only a subset of all connection parameters are allowed.
-- I wish this function existed in postgresql-simple or some form of it in postgresql-libpq, but if it does I couldn't find it.
connStringParser :: Parser ConnString
connStringParser = do
  connStr <-
    Parsec.takeWhile1 (const True)
      <|> fail "Empty connection string"
  -- Very poor connection string type handling here
  let connStrParser =
        if ("postgres://" `Text.isPrefixOf` Text.toLower connStr)
          || ("postgresql://" `Text.isPrefixOf` Text.toLower connStr)
          then
            uriConnParser
          else
            keywordValueConnParser
  case connStrParser connStr of
    Left err ->
      fail $
        "Connection string is not a valid libpq connection string. A valid libpq connection string is either in the format 'postgres://username[:password]@host:port/database_name', with URI-encoded (percent-encoded) components except for the host and bracket-surround IPv6 addresses, or in the keyword value pairs format, e.g. 'dbname=database_name host=localhost user=postgres' with escaping for spaces, quotes or empty values. More info at https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING. Specific error: "
          <> err
    Right c -> pure c
