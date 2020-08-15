module HPgsql.Msgs (AuthenticationOk (..), BackendKeyData (..), Bind (..), BindComplete (..), CancelRequest (..), CommandComplete (..), CopyData (..), CopyDone (..), CopyFail (..), CopyInResponse (..), DataRow (..), Describe (..), ErrorDetail (..), ErrorResponse (..), Execute (..), Flush (..), NoData (..), ParameterStatus (..), Query (..), ReadyForQuery (..), RowDescription (..), StartupMessage (..), ToPgMessage (..), FromPgMessage (..), PgMsgParser (..), Terminate (..), TransactionStatus (..), NoticeResponse (..), NotificationResponse (..), Parse (..), ParseComplete (..), Sync (..), parsePgMessage) where

-- TODO: Make this whole module internal!

import Control.Applicative (Alternative (..))
import Control.Monad (replicateM)
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.Attoparsec.ByteString.Lazy as LazyParsec
import qualified Data.Attoparsec.Text as TextParsec
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Int (Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII, decodeUtf8)
import Data.Word (Word8)
import HPgsql.TypeInfo (Format (..), Oid (..))

class ToPgMessage a where
  toPgMessage :: a -> Builder

newtype PgMsgParser a
  = PgMsgParser
      ( Char ->
        -- \| Message contents after the Int32 length attribute
        LBS.ByteString ->
        Maybe a
      )
  deriving stock (Functor)

instance Applicative PgMsgParser where
  pure a = PgMsgParser $ \_ _ -> Just a

  -- TODO: Is this Applicative correct? Double-check laws
  PgMsgParser f <*> PgMsgParser p = PgMsgParser $ \c r -> f c r <*> p c r

instance Alternative PgMsgParser where
  empty = PgMsgParser $ \_ _ -> Nothing
  PgMsgParser p1 <|> PgMsgParser p2 = PgMsgParser $ \c restOfMsg -> p1 c restOfMsg <|> p2 c restOfMsg

class FromPgMessage a where
  msgParser :: PgMsgParser a

parsePgMessage :: Char -> LBS.ByteString -> PgMsgParser a -> Maybe a
parsePgMessage c restOfMsg (PgMsgParser parseFunc) = parseFunc c restOfMsg

colParser :: Parsec.Parser Oid
colParser = do
  Parsec.skipWhile (/= 0) -- Column name as C string
  Parsec.skip (== 0)
  void $ Parsec.take (4 + 2)
  -- TODO: OIDs are unsigned integers! Try `select (-1)::oid` to see. Change to UInt32 somehow
  typOid <- Binary.decode @Int32 . BS.fromStrict <$> Parsec.take 4
  void $ Parsec.take (2 + 4 + 2)
  pure $ Oid (fromIntegral typOid)

nulTerminatedCStringParser :: Parsec.Parser Text
nulTerminatedCStringParser = do
  stringWithoutNul <- Parsec.takeWhile (/= 0)
  Parsec.skip (== 0)
  pure $ decodeUtf8 stringWithoutNul

data AuthenticationOk = AuthenticationOk
  deriving stock (Show)

data BackendKeyData = BackendKeyData {backendPid :: Int32, backendSecretKey :: Int32}
  deriving stock (Show)

data Bind = Bind {paramsValuesInOrder :: [Maybe LBS.ByteString], resultColumnFmts :: [Format]}
  deriving stock (Show)

data BindComplete = BindComplete
  deriving stock (Show)

-- | PId first, secret key second
data CancelRequest = CancelRequest Int32 Int32
  deriving stock (Show)

newtype CommandComplete = CommandComplete {numRows :: Int64}
  deriving stock (Show)

newtype CopyData = CopyData ByteString

instance Show CopyData where
  show _ = "CopyData"

data Describe = Describe
  deriving stock (Show)

newtype ErrorResponse = ErrorResponse (Map ErrorDetail LBS.ByteString)
  deriving stock (Show)

data Execute = Execute
  deriving stock (Show)

data CopyDone = CopyDone
  deriving stock (Show)

newtype CopyFail = CopyFail {causeForFailure :: String}
  deriving stock (Show)

data CopyInResponse = CopyInResponse
  deriving stock (Show)

data Flush = Flush
  deriving stock (Show)

data NoData = NoData
  deriving stock (Show)

newtype NoticeResponse = NoticeResponse (Map ErrorDetail LBS.ByteString)
  deriving stock (Show)

data NotificationResponse = NotificationResponse {notifierPid :: !Int32, channelName :: !Text, notifPayload :: !Text}
  deriving stock (Eq, Show)

newtype Query = Query ByteString

instance Show Query where
  show _ = "Query"

newtype DataRow = DataRow {rowColumnData :: LBS.ByteString}

instance Show DataRow where
  show _ = "DataRow"

data Parse = Parse {queryString :: ByteString, specifiedParameterTypes :: [Maybe Oid]}

instance Show Parse where
  show Parse {..} = "Parse (" ++ show (length specifiedParameterTypes) ++ " params specified) - " ++ show queryString

data ParseComplete = ParseComplete
  deriving stock (Show)

data ParameterStatus = ParameterStatus {parameterName :: Text, parameterValue :: Text}
  deriving stock (Show)

-- | This replicates the postgresql-libpq constructor, because why not?
data TransactionStatus = TransIdle | TransInTrans | TransInError
  deriving stock (Eq, Show)

newtype ReadyForQuery
  = ReadyForQuery TransactionStatus
  deriving stock (Show)

newtype RowDescription = RowDescription {resultColumnTypes :: [Oid]}
  deriving stock (Show)

data StartupMessage = StartupMessage {user :: String, database :: String, options :: String}
  deriving stock (Show)

data Sync = Sync
  deriving stock (Show)

data Terminate = Terminate
  deriving stock (Show)

instance FromPgMessage AuthenticationOk where
  msgParser = PgMsgParser $ \c _restOfMsg -> case c of
    'R' -> Just AuthenticationOk
    _ -> Nothing

instance FromPgMessage BackendKeyData where
  msgParser = PgMsgParser $ \c (LBS.splitAt 4 -> (pidBS, secretBS)) -> case c of
    'K' -> Just $ BackendKeyData {backendPid = Binary.decode @Int32 pidBS, backendSecretKey = Binary.decode @Int32 secretBS}
    _ -> Nothing

instance FromPgMessage BindComplete where
  msgParser = PgMsgParser $ \c _restOfMsg -> case c of
    '2' -> Just BindComplete
    _ -> Nothing

instance FromPgMessage CommandComplete where
  msgParser = PgMsgParser $ \c restOfMsg -> case c of
    'C' ->
      let astext = decodeASCII $ LBS.toStrict $ LBS.dropEnd 1 restOfMsg
       in case TextParsec.parseOnly ((ins <|> del <|> upd <|> merge <|> sel <|> move <|> fetch <|> copy) <* TextParsec.endOfInput) astext of
            Left _ -> Just $ CommandComplete 0
            Right n -> Just $ CommandComplete n
    _ -> Nothing
    where
      ins = TextParsec.string "INSERT 0 " >> TextParsec.decimal
      del = TextParsec.string "DELETE " >> TextParsec.decimal
      upd = TextParsec.string "UPDATE " >> TextParsec.decimal
      merge = TextParsec.string "MERGE " >> TextParsec.decimal
      sel = TextParsec.string "SELECT " >> TextParsec.decimal
      move = TextParsec.string "MOVE " >> TextParsec.decimal
      fetch = TextParsec.string "FETCH " >> TextParsec.decimal
      copy = TextParsec.string "COPY " >> TextParsec.decimal

instance ToPgMessage CancelRequest where
  toPgMessage (CancelRequest pid secret) =
    Builder.int32BE (4 + 4 + 4 + 4) <> Builder.int32BE 80877102 <> Builder.int32BE pid <> Builder.int32BE secret

instance ToPgMessage CopyData where
  toPgMessage (CopyData bs) =
    -- TODO: Do we check if bytestring's length is too long or just continue ignoring the possibility?
    Builder.char7 'd' <> Builder.int32BE (4 + fromIntegral (BS.length bs)) <> Builder.byteString bs

instance ToPgMessage CopyFail where
  toPgMessage (CopyFail bs) =
    let cstr = nulTermCString bs
     in Builder.char7 'f' <> Builder.int32BE (4 + fromIntegral (length bs) + 1) <> cstr

instance ToPgMessage CopyDone where
  toPgMessage CopyDone =
    Builder.char7 'c' <> Builder.int32BE 4

instance ToPgMessage Describe where
  toPgMessage Describe =
    let unnamedPortal = nulTermCString ""
     in Builder.char7 'D' <> Builder.int32BE (4 + 1 + 1) <> Builder.char7 'P' <> unnamedPortal

instance ToPgMessage Execute where
  toPgMessage _ =
    let unnamedPortal = nulTermCString ""
        infiniteRowsToReturn :: Int32 = 0
        contents = unnamedPortal <> Builder.int32BE infiniteRowsToReturn
        contentsLen = fromIntegral $ LBS.length $ Builder.toLazyByteString contents
     in Builder.char7 'E' <> Builder.int32BE (4 + contentsLen) <> contents

instance ToPgMessage Flush where
  toPgMessage Flush =
    Builder.char7 'H' <> Builder.int32BE 4

instance FromPgMessage CopyInResponse where
  msgParser = PgMsgParser $ \c _ -> case c of
    'G' -> Just CopyInResponse
    _ -> Nothing

instance FromPgMessage DataRow where
  msgParser = PgMsgParser $ \c restOfMsg -> case c of
    'D' -> Just $ DataRow {rowColumnData = LBS.drop 2 restOfMsg}
    _ -> Nothing

instance FromPgMessage NoData where
  msgParser = PgMsgParser $ \c _ -> case c of
    'n' -> Just NoData
    _ -> Nothing

instance FromPgMessage ParameterStatus where
  msgParser = PgMsgParser $ \c restOfMsg -> case c of
    'S' -> case LazyParsec.parseOnly (((,) <$> nulTerminatedCStringParser <*> nulTerminatedCStringParser) <* Parsec.endOfInput) restOfMsg of
      Left _ -> error "Failed parsing ParameterStatus"
      Right (parameterName, parameterValue) -> Just $ ParameterStatus {..}
    _ -> Nothing

instance FromPgMessage ParseComplete where
  msgParser = PgMsgParser $ \c _ -> case c of
    '1' -> Just ParseComplete
    _ -> Nothing

instance ToPgMessage Query where
  toPgMessage (Query bs) =
    -- TODO: Do we check if bytestring's length is too long or just continue ignoring the possibility?
    Builder.char7 'Q' <> Builder.int32BE (5 + fromIntegral (BS.length bs)) <> Builder.byteString bs <> Builder.word8 0

instance ToPgMessage Sync where
  toPgMessage Sync =
    Builder.char7 'S' <> Builder.int32BE 4

instance ToPgMessage Terminate where
  toPgMessage Terminate =
    Builder.char7 'X' <> Builder.int32BE 4

nulTermCString :: String -> Builder
nulTermCString s = Builder.string7 s <> Builder.word8 0

instance ToPgMessage Bind where
  toPgMessage Bind {..} =
    let unnamedDestPortal = nulTermCString ""
        unnamedSourcePreparedStmt = nulTermCString ""
        numParamFmtCodesAllBinary :: Int16 = 1
        fmtCodesBinary :: Int16 = 1
        numQryParams :: Int16 = fromIntegral $ length paramsValuesInOrder
        paramsLenAndVals =
          mconcat $
            map
              ( \case
                  Nothing -> Builder.int32BE (-1)
                  Just val -> Builder.int32BE (fromIntegral $ LBS.length val) <> Builder.lazyByteString val
              )
              paramsValuesInOrder
        numResultColumnsFmtCodes :: Int16 = fromIntegral $ length resultColumnFmts
        resultColumnsFmtCodes =
          mconcat $
            map
              ( \case
                  BinaryFmt -> Builder.int16BE 1
                  TextFmt -> Builder.int16BE 0
              )
              resultColumnFmts
        contents = unnamedDestPortal <> unnamedSourcePreparedStmt <> Builder.int16BE numParamFmtCodesAllBinary <> Builder.int16BE fmtCodesBinary <> Builder.int16BE numQryParams <> paramsLenAndVals <> Builder.int16BE numResultColumnsFmtCodes <> resultColumnsFmtCodes
        contentsLen = fromIntegral $ LBS.length $ Builder.toLazyByteString contents
     in Builder.char7 'B' <> Builder.int32BE (4 + contentsLen) <> contents

instance ToPgMessage Parse where
  toPgMessage Parse {..} =
    let unnamedStatement = nulTermCString ""
        numParamsSpecified :: Int16 = fromIntegral $ length specifiedParameterTypes
        -- A 0 for an OID means "type unspecified", and is what we do for custom user types
        paramOids = mconcat $ map (\mOid -> Builder.int32BE $ fromMaybe 0 $ fromIntegral <$> mOid) specifiedParameterTypes
        contents = unnamedStatement <> Builder.byteString queryString <> Builder.word8 0 <> Builder.int16BE numParamsSpecified <> paramOids
        contentsLen = fromIntegral $ LBS.length $ Builder.toLazyByteString contents
     in Builder.char7 'P' <> Builder.int32BE (4 + contentsLen) <> contents

instance ToPgMessage StartupMessage where
  toPgMessage StartupMessage {..} =
    let protocolMajorVersion :: Int16 = 3
        protocolMinorVersion :: Int16 = 0
        userBS = nulTermCString "user" <> nulTermCString user
        databaseBS = nulTermCString "database" <> nulTermCString database
        optionsBS = nulTermCString "options" <> nulTermCString options
        contents = Builder.int16BE protocolMajorVersion <> Builder.int16BE protocolMinorVersion <> userBS <> databaseBS <> optionsBS <> Builder.word8 0
        contentsLen = fromIntegral $ LBS.length $ Builder.toLazyByteString contents
     in -- TODO: What protocol version do we announce? We're following the example from the docs for now
        -- TODO: Can we send things like client_encoding, DateStyle, et. al as options here?

        Builder.int32BE (4 + contentsLen) <> contents

instance FromPgMessage ReadyForQuery where
  msgParser = PgMsgParser $ \c restOfMsg -> case (c, restOfMsg) of
    ('Z', "I") -> Just $ ReadyForQuery TransIdle
    ('Z', "T") -> Just $ ReadyForQuery TransInTrans
    ('Z', "E") -> Just $ ReadyForQuery TransInError
    _ -> Nothing

instance FromPgMessage RowDescription where
  msgParser = PgMsgParser $ \c restOfMsg ->
    if c == 'T'
      then
        let (numColsBS, colContents) = LBS.splitAt 2 restOfMsg
            numCols = Binary.decode @Int16 numColsBS
            allColOidsParser :: Parsec.Parser [Oid]
            allColOidsParser = replicateM (fromIntegral numCols) colParser
         in case LazyParsec.parseOnly (allColOidsParser <* Parsec.endOfInput) colContents of
              Left err -> error $ "Error parsing row's column types' OIDs: " ++ err
              Right v -> Just $ RowDescription v
      else Nothing

instance FromPgMessage ErrorResponse where
  msgParser = PgMsgParser $ \c restOfMsg ->
    if c /= 'E'
      then Nothing
      else
        if LBS.length restOfMsg == 0
          then Just $ ErrorResponse mempty
          else
            let errorFields = LBS.split 0 restOfMsg
                parseSingleErrorField ef = do
                  -- Maybe monad
                  (fieldErrorByte, fieldErrorBs) <- LBS.uncons ef
                  errDetail <- byteToErrorDetail fieldErrorByte
                  pure (errDetail, fieldErrorBs)
             in Just $ ErrorResponse $ Map.fromList $ mapMaybe parseSingleErrorField errorFields

instance FromPgMessage NoticeResponse where
  msgParser = PgMsgParser $ \c restOfMsg ->
    if c /= 'N'
      then Nothing
      else
        -- TODO NOTICEs are exactly like ErrorResponses, so we could use a single parser
        if LBS.length restOfMsg == 0
          then Just $ NoticeResponse mempty
          else
            let errorFields = LBS.split 0 restOfMsg
                parseSingleErrorField ef = do
                  -- Maybe monad
                  (fieldErrorByte, fieldErrorBs) <- LBS.uncons ef
                  errDetail <- byteToErrorDetail fieldErrorByte
                  pure (errDetail, fieldErrorBs)
             in Just $ NoticeResponse $ Map.fromList $ mapMaybe parseSingleErrorField errorFields

instance FromPgMessage NotificationResponse where
  msgParser = PgMsgParser $ \c restOfMsg ->
    if c /= 'A'
      then Nothing
      else
        let (notifierPidBs, channelNameAndPayload) = LBS.splitAt 4 restOfMsg
            notifierPid = Binary.decode @Int32 notifierPidBs
         in case LazyParsec.parseOnly
              ((NotificationResponse notifierPid <$> nulTerminatedCStringParser <*> nulTerminatedCStringParser) <* Parsec.endOfInput)
              channelNameAndPayload of
              Left _ -> Nothing
              Right notif -> Just notif

data ErrorDetail
  = ErrorSeverity
  | ErrorCode
  | ErrorHumanReadableMsg
  | ErrorDetail
  | ErrorHint
  | ErrorPosition
  | ErrorInternalPosition
  | ErrorInternalCommand
  | ErrorContext
  | ErrorSchema
  | ErrorTable
  | ErrorColumn
  | ErrorType
  | ErrorConstraint
  | ErrorSourceFile
  | ErrorSourceLine
  | ErrorSourceRoutine
  deriving stock (Eq, Ord, Show)

byteToErrorDetail :: Word8 -> Maybe ErrorDetail
byteToErrorDetail b = case w2c b of
  -- Taken from https://www.postgresql.org/docs/current/protocol-error-fields.html
  'S' -> Nothing -- This is localized severity which can make code locale-dependent, so we don't want it!
  'V' -> Just ErrorSeverity
  -- S

  --     Severity: the field contents are ERROR, FATAL, or PANIC (in an error message), or WARNING, NOTICE, DEBUG, INFO, or LOG (in a notice message), or a localized translation of one of these. Always present.
  -- V

  --     Severity: the field contents are ERROR, FATAL, or PANIC (in an error message), or WARNING, NOTICE, DEBUG, INFO, or LOG (in a notice message). This is identical to the S field except that the contents are never localized. This is present only in messages generated by PostgreSQL versions 9.6 and later.
  'C' -> Just ErrorCode
  -- C

  --     Code: the SQLSTATE code for the error (see Appendix A). Not localizable. Always present.
  'M' -> Just ErrorHumanReadableMsg
  -- M

  --     Message: the primary human-readable error message. This should be accurate but terse (typically one line). Always present.
  'D' -> Just ErrorDetail
  -- D

  --     Detail: an optional secondary error message carrying more detail about the problem. Might run to multiple lines.
  'H' -> Just ErrorHint
  -- H

  --     Hint: an optional suggestion what to do about the problem. This is intended to differ from Detail in that it offers advice (potentially inappropriate) rather than hard facts. Might run to multiple lines.
  'P' -> Just ErrorPosition
  -- P

  --     Position: the field value is a decimal ASCII integer, indicating an error cursor position as an index into the original query string. The first character has index 1, and positions are measured in characters not bytes.
  'p' -> Just ErrorInternalPosition
  -- p

  --     Internal position: this is defined the same as the P field, but it is used when the cursor position refers to an internally generated command rather than the one submitted by the client. The q field will always appear when this field appears.
  'q' -> Just ErrorInternalCommand
  -- q

  --     Internal query: the text of a failed internally-generated command. This could be, for example, an SQL query issued by a PL/pgSQL function.
  'W' -> Just ErrorContext
  -- W

  --     Where: an indication of the context in which the error occurred. Presently this includes a call stack traceback of active procedural language functions and internally-generated queries. The trace is one entry per line, most recent first.
  's' -> Just ErrorSchema
  -- s

  --     Schema name: if the error was associated with a specific database object, the name of the schema containing that object, if any.
  't' -> Just ErrorTable
  -- t

  --     Table name: if the error was associated with a specific table, the name of the table. (Refer to the schema name field for the name of the table's schema.)
  'c' -> Just ErrorColumn
  -- c

  --     Column name: if the error was associated with a specific table column, the name of the column. (Refer to the schema and table name fields to identify the table.)
  'd' -> Just ErrorType
  -- d

  --     Data type name: if the error was associated with a specific data type, the name of the data type. (Refer to the schema name field for the name of the data type's schema.)
  'n' -> Just ErrorConstraint
  -- n

  --     Constraint name: if the error was associated with a specific constraint, the name of the constraint. Refer to fields listed above for the associated table or domain. (For this purpose, indexes are treated as constraints, even if they weren't created with constraint syntax.)
  'F' -> Just ErrorSourceFile
  -- F

  --     File: the file name of the source-code location where the error was reported.
  'L' -> Just ErrorSourceLine
  -- L

  --     Line: the line number of the source-code location where the error was reported.
  'R' -> Just ErrorSourceRoutine
  -- R

  --     Routine: the name of the source-code routine reporting the error.
  _ -> Nothing
