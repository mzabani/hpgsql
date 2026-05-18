module Hpgsql.ScramSHA256
  ( ScramClientFirstMessage (..),
    ScramServerFirstMessage (..),
    ScramClientFinalMessage (..),
    generateClientFirstMessage,
    handleServerFirstMsg,
    verifyServerFinal,
  )
where

-- https://datatracker.ietf.org/doc/html/rfc5802#section-3
-- The RFC for SCRAM-SHA-1 (which SCRAM-SHA-256 follows except it uses SHA256)
-- says this:
--
-- "Informative Note: Implementors are encouraged to create test cases
--    that use both usernames and passwords with non-ASCII codepoints.  In
--    particular, it's useful to test codepoints whose "Unicode
--    Normalization Form C" and "Unicode Normalization Form KC" are
--    different.  Some examples of such codepoints include Vulgar Fraction
--    One Half (U+00BD) and Acute Accent (U+00B4)."
--
-- TODO: We should test these scenarios, but we don't yet.

import Control.Applicative ((<|>))
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Random (getRandomBytes)
import Data.Attoparsec.ByteString (parseOnly, string)
import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import Data.ByteArray (convert, xor)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

-- From the RFC:
-- SCRAM is a SASL mechanism whose client response and server challenge
--    messages are text-based messages containing one or more attribute-
--    value pairs separated by commas.  Each attribute has a one-letter
--    name.  The messages and their attributes are described in
--    Section 5.1, and defined in Section 7.

--    SCRAM is a client-first SASL mechanism (see [RFC4422], Section 5,
--    item 2a), and returns additional data together with a server's
--    indication of a successful outcome.

--    This is a simple example of a SCRAM-SHA-1 authentication exchange
--    when the client doesn't support channel bindings (username 'user' and
--    password 'pencil' are used):

--    C: n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL
--    S: r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,
--       i=4096
--    C: c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,
--       p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=
--    S: v=rmF9pqV8S7suAoZWja4dJRkFsKQ=
--
--
-- And from Postgres (https://www.postgresql.org/docs/current/sasl-authentication.html):
-- 1. The server sends an AuthenticationSASL message. It includes a list of SASL authentication mechanisms that the server can accept. This will be SCRAM-SHA-256-PLUS and SCRAM-SHA-256 if the server is built with SSL support, or else just the latter.
-- 2. The client responds by sending a SASLInitialResponse message, which indicates the chosen mechanism, SCRAM-SHA-256 or SCRAM-SHA-256-PLUS. (A client is free to choose either mechanism, but for better security it should choose the channel-binding variant if it can support it.) In the Initial Client response field, the message contains the SCRAM client-first-message. The client-first-message also contains the channel binding type chosen by the client.
--      TODO: hpgsql right now only supports SCRAM-SHA-256, not the -PLUS variant.
-- 3. Server sends an AuthenticationSASLContinue message, with a SCRAM server-first-message as the content.
-- 4. Client sends a SASLResponse message, with SCRAM client-final-message as the content.
-- 5. Server sends an AuthenticationSASLFinal message, with the SCRAM server-final-message, followed immediately by an AuthenticationOk message.

-- | The "client-first-message", something like `n,,n=user,r=nonce`, where
-- the nonce is something like `fyko+d2lbbFgONRv9qkxdawL`.
data ScramClientFirstMessage = ScramClientFirstMessage
  { -- | This is the string "n,,"
    gs2Header :: !ByteString,
    -- | This is the string "n=user,r=nonce", except that "user" is the
    -- empty string because postgres ignores it anyway.
    clientFirstBare :: !ByteString,
    -- | This is just the concatenation of the two strings above.
    fullMessage :: !ByteString
  }

-- | Generate the client-first-message for SCRAM-SHA-256.
generateClientFirstMessage :: IO ScramClientFirstMessage
generateClientFirstMessage = do
  -- We don't send a username in this message even if it's necessary according
  -- to the RFC, because postgres says
  -- "When SCRAM-SHA-256 is used in PostgreSQL, the server will ignore the user name that the client sends in the client-first-message. The user name that was already sent in the startup message is used instead."
  nonce <- generateNonce
  let gs2Header = "n,,"
      clientFirstBare = "n=,r=" <> nonce
      fullMessage = gs2Header <> clientFirstBare
  pure ScramClientFirstMessage {..}
  where
    generateNonce :: IO ByteString
    generateNonce = convertToBase Base64 <$> (getRandomBytes 24 :: IO ByteString)

newtype ScramServerFirstMessage = ScramServerFirstMessage
  { -- | As per the RFC, this looks something like (notice the concatenation
    -- of client and server nonces, and some salt):
    --    S: r=ClientNonceServerNonce,s=QSXCR+Q6sek8bf92,
    --       i=4096
    fullMessage :: ByteString
  }
  deriving stock (Show)

data ScramClientFinalMessage = ScramClientFinalMessage
  { -- | This looks something like:
    -- c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,
    --       p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=
    clientFinalMessage :: !ByteString,
    serverSignature :: !ByteString
  }
  deriving stock (Show)

-- | From the RFC:
-- In response, the server sends a "server-first-message" containing the
-- user's iteration count i and the user's salt, and appends its own
-- nonce to the client-specified one.

-- The client then responds by sending a "client-final-message" with the
-- same nonce and a ClientProof computed using the selected hash
-- function as explained earlier.
handleServerFirstMsg ::
  Text ->
  ScramClientFirstMessage ->
  ScramServerFirstMessage ->
  Either String ScramClientFinalMessage
handleServerFirstMsg (encodeUtf8 -> password) ScramClientFirstMessage {gs2Header, clientFirstBare} serverFirst = do
  (serverNonce, salt, iterations) <- parseServerFirst serverFirst.fullMessage
  let saltedPassword = hi salt iterations -- "hi" takes the password implicitly
      clientKey = hmacSHA256 saltedPassword "Client Key"
      storedKey = hashSHA256 clientKey
      channelBinding = "c=" <> convertToBase Base64 gs2Header
      clientFinalWithoutProof = channelBinding <> ",r=" <> serverNonce
      authMessage = clientFirstBare <> "," <> serverFirst.fullMessage <> "," <> clientFinalWithoutProof
      clientSignature = hmacSHA256 storedKey authMessage
      clientProof = Data.ByteArray.xor clientKey clientSignature :: ByteString
      clientFinalMessage = clientFinalWithoutProof <> ",p=" <> convertToBase Base64 clientProof
      serverKey = hmacSHA256 saltedPassword "Server Key"
      serverSignature = convertToBase Base64 (hmacSHA256 serverKey authMessage) :: ByteString
  pure ScramClientFinalMessage {clientFinalMessage, serverSignature}
  where
    parseServerFirst :: ByteString -> Either String (ByteString, ByteString, Int)
    parseServerFirst = parseOnly $ do
      -- As per the RFC, this looks something like (notice the concatenation
      -- of client and server nonces, and some salt):
      --    S: r=ClientNonceServerNonce,s=QSXCR+Q6sek8bf92,
      --       i=4096
      void $ string "r=" <|> fail "Missing nonce in server-first-message (1)"
      nonce <- Parsec.takeWhile1 (/= ',') <|> fail "Missing nonce in server-first-message (2)"
      void $ string ","
      void $ string "s=" <|> fail "Missing salt in server-first-message (1)"
      saltB64 <- Parsec.takeWhile1 (/= ',') <|> fail "Missing salt in server-first-message (2)"
      void $ string ","
      salt <- case convertFromBase Base64 saltB64 of
        Left e -> fail $ "Error converting salt from base64: " ++ e
        Right s -> pure s
      void $ string "i=" <|> fail "Missing iteration count in server-first-message"
      iters <- Parsec.decimal <|> fail "Iteration count not a number in server-first-message"
      Parsec.endOfInput
      pure (nonce, salt, iters)
    hi :: ByteString -> Int -> ByteString
    hi salt iterations =
      fastPBKDF2_SHA256
        Parameters {iterCounts = iterations, outputLength = 32}
        password
        salt

-- | Verify the server-final-message matches the expected server signature.
verifyServerFinal :: ScramClientFinalMessage -> ByteString -> Either String ()
verifyServerFinal finalMsg serverFinal = do
  actualSig <- parseServerFinal serverFinal
  if actualSig == finalMsg.serverSignature
    then Right ()
    else Left "Server signature mismatch"
  where
    parseServerFinal :: ByteString -> Either String ByteString
    parseServerFinal msg
      -- This looks like:
      --   v=rmF9pqV8S7suAoZWja4dJRkFsKQ=
      -- And v is a base64-encoded ServerSignature, or it looks like
      --   e=some-error
      | BS.isPrefixOf "v=" msg = Right (BS.drop 2 msg)
      | BS.isPrefixOf "e=" msg = Left $ "Server error: " <> BS8.unpack (BS.drop 2 msg)
      | otherwise = Left $ "Invalid server-final-message: " <> BS8.unpack msg

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = convert $ hmacGetDigest (hmac key msg :: HMAC SHA256)

hashSHA256 :: ByteString -> ByteString
hashSHA256 bs = convert (hash bs :: Digest SHA256)
