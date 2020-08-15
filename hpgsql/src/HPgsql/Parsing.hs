-- |
--
-- This module contains parsers that are helpful to separate SQL statements from each other by finding query boundaries: semi-colons, but not when inside a string or a parenthesised expression, for example.
module HPgsql.Parsing
  ( parseSql,
    piecesToText,
    SqlPiece(..),
    BlockOrNotBlock(..),
  )
where

import Control.Applicative
  ( optional,
    (<|>),
  )
import Control.Monad
  ( void,
    when,
  )
import Data.Attoparsec.Text
  ( Parser,
    asciiCI,
    char,
    endOfInput,
    many',
    many1,
    peekChar,
    skipWhile,
    string,
    takeWhile,
    takeWhile1,
  )
import qualified Data.Attoparsec.Text as Parsec
import Data.Bifunctor (first)
import qualified Data.Char as Char
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Streaming
  ( Identity (..),
    Of (..),
  )
import qualified Streaming.Internal as S
import Streaming.Prelude (Stream)
import qualified Streaming.Prelude as Streaming
import Prelude hiding (takeWhile)

data SqlPiece = CommentPiece !Text | WhiteSpacePiece !Text | CopyFromStdinStatement !Text | CopyFromStdinRows !Text | CopyFromStdinEnd !Text | BeginTransaction !Text | OtherSqlPiece ![BlockOrNotBlock]
  deriving stock (Show, Eq)

-- | Blocks are the name we give to some expressions that have a beginning and an end, inside of which
-- semicolons are not to be considered statement boundaries. These include strings, comments,
-- parenthesised expressions and dollar-quoted strings.
-- Knowing if a fragment of SQL is a block or not can help interpolate '?' as arguments in some contexts,
-- while avoiding doing so inside e.g. text strings.
data BlockOrNotBlock = Block !Text | NotBlock !Text
  deriving stock (Eq, Show)

parseSql :: String -> [SqlPiece]
parseSql str = runIdentity $ Streaming.toList_ $ parseSqlPiecesStreaming' sqlPieceParser (Streaming.yield (Text.pack str))

parseSqlPiecesStreaming' ::
  forall m.
  (Monad m) =>
  (ParserState -> Parser ([SqlPiece], ParserState)) ->
  Stream (Of Text) m () ->
  Stream (Of SqlPiece) m ()
parseSqlPiecesStreaming' parser contents = go $ Streaming.concat $ manyStreaming parser OutsideCopy contents
  where
    go :: Stream (Of SqlPiece) m (Stream (Of Text) m (), ParserState) -> Stream (Of SqlPiece) m ()
    go = \case
      S.Step (sqlPiece :> rest) -> S.Step $ sqlPiece :> go rest
      S.Return (unparsedTextStream, _) -> S.Effect $ do
        allRemainingText <- Streaming.mconcat_ unparsedTextStream
        -- If there is white-space at the end of the migration, it would've been parsed as WhiteSpacePiece. If there is valid SQL, then it would've been parsed as some other SQL piece. And so on.
        -- Thus, if we're here, the end of the migration is either empty text or something we failed to parse. It is almost certainly
        -- the former, but if it's the latter we return it all in a single OtherSqlPiece to try to be helpful.
        if allRemainingText == ""
          then pure $ S.Return ()
          else
            pure $ Streaming.yield $ OtherSqlPiece [NotBlock allRemainingText]
      S.Effect eff -> S.Effect $ go <$> eff

-- TODO: Remove dlist as a dependency if we can

-- | This should be equivalent to attoparsec's `many`, but with streams and a stateful parser. Naturally, there are differences in the type signature given the Streaming nature of this function.
-- It returns as the Stream's result the unparsed text.
manyStreaming ::
  forall m a s.
  (Monad m, Show a) =>
  (s -> Parser (a, s)) ->
  s ->
  -- | The input stream/text. Empty strings are ignored, and end-of-stream interpreted as an EOF marker when parsing.
  Stream (Of Text) m () ->
  -- | Returns a stream of parsed values with the stream's result being any unparsed text at the end.
  Stream (Of a) m (Stream (Of Text) m (), s)
manyStreaming parser initialState inputStream = go initialState DList.empty (Parsec.parse (parser initialState)) inputStreamWithoutEmptyStrs
  where
    -- End-Of-Stream is EOF for us, but attoparsec understands the empty string as EOF, so we filter it out because empty strings
    -- are perfectly valid chunks for streams and should have no impact on parsing
    inputStreamWithoutEmptyStrs = Streaming.filter ("" /=) inputStream
    go :: s -> DList.DList Text -> (Text -> Parsec.Result (a, s)) -> Stream (Of Text) m () -> Stream (Of a) m (Stream (Of Text) m (), s)
    go s !partiallyParsedTexts parseFunc stream =
      case stream of
        S.Step (textPiece :> rest) -> case parseFunc textPiece of
          Parsec.Fail {} -> S.Return (Streaming.each (DList.toList partiallyParsedTexts) <> Streaming.yield textPiece <> rest, s)
          Parsec.Done unconsumedInput (parsedValue, newParserState) -> S.Step $ parsedValue :> go newParserState DList.empty (Parsec.parse (parser newParserState)) (if unconsumedInput == "" then rest else S.Step $ unconsumedInput :> rest)
          Parsec.Partial continueParsing -> go s (partiallyParsedTexts `DList.snoc` textPiece) continueParsing rest
        S.Effect m -> S.Effect $ go s partiallyParsedTexts parseFunc <$> m
        S.Return () ->
          -- End of stream is EOF, which is represented by the empty string for attoparsec parsers
          case parseFunc "" of
            Parsec.Fail {} ->
              S.Return (Streaming.each $ DList.toList partiallyParsedTexts, s)
            Parsec.Done !unconsumedInput (!parsedValue, !newParserState) -> S.Step $ parsedValue :> S.Return (Streaming.yield unconsumedInput, newParserState)
            Parsec.Partial _ ->
              -- What is this case? Partial match on EOF? I suppose it is possible for an arbitrary parser to do this..
              S.Return (Streaming.each $ DList.toList partiallyParsedTexts, s)

sqlPieceText :: SqlPiece -> Text
sqlPieceText (CommentPiece s) = s
sqlPieceText (WhiteSpacePiece s) = s
sqlPieceText (BeginTransaction s) = s
sqlPieceText (OtherSqlPiece ls) = Text.concat $ map blockText ls
sqlPieceText (CopyFromStdinStatement s) = s
sqlPieceText (CopyFromStdinRows s) = s
sqlPieceText (CopyFromStdinEnd s) = s

mapSqlPiece :: (Text -> Text) -> SqlPiece -> SqlPiece
mapSqlPiece f = \case
  CommentPiece s -> CommentPiece (f s)
  WhiteSpacePiece s -> WhiteSpacePiece (f s)
  BeginTransaction s -> BeginTransaction (f s)
  OtherSqlPiece s -> OtherSqlPiece (map (mapBlock f) s)
  CopyFromStdinStatement s -> CopyFromStdinStatement (f s)
  CopyFromStdinRows s -> CopyFromStdinRows (f s)
  CopyFromStdinEnd s -> CopyFromStdinEnd (f s)

data ParserState = OutsideCopy | InsideCopy
  deriving stock (Eq, Show)

sqlPieceParser :: ParserState -> Parser ([SqlPiece], ParserState)
sqlPieceParser parserState = case parserState of
  OutsideCopy -> first (: []) <$> outsideCopyParser
  InsideCopy -> copyFromStdinAfterStatementParser 65536
  where
    outsideCopyParser =
      (,OutsideCopy)
        . CommentPiece
        . blockText
        <$> commentParser
        <|> (,OutsideCopy)
          . WhiteSpacePiece
          <$> takeWhile1
            (\c -> Char.isSpace c || c == '\n' || c == '\r' || c == '\t')
        <|> (,InsideCopy)
          <$> copyFromStdinStatementParser
        <|> (,OutsideCopy)
          . BeginTransaction
          . blockListText
          <$> beginTransactionParser
        <|> (,OutsideCopy)
          . OtherSqlPiece
          <$> anySqlPieceParser
    beginTransactionParser =
      spaceSeparatedTokensToParser
        [CITextToken "BEGIN", AllUntilEndOfStatement]
        <|> spaceSeparatedTokensToParser
          [ CITextToken "START",
            CITextToken "TRANSACTION",
            AllUntilEndOfStatement
          ]
    anySqlPieceParser = do
      arbText <- spaceSeparatedTokensToParser [AllUntilEndOfStatement]
      when (blockListText arbText == "") $ fail "Please report this as a bug in codd: trying to parse empty string as SQL piece"
      pure arbText

data SqlToken = CITextToken !Text | SqlIdentifier | CommaSeparatedIdentifiers | Optional ![SqlToken] | CustomParserToken (Parser [BlockOrNotBlock]) | AllUntilEndOfStatement

spaceSeparatedTokensToParser :: [SqlToken] -> Parser [BlockOrNotBlock]
spaceSeparatedTokensToParser allTokens = case allTokens of
  [] -> pure []
  [token1] -> parseToken token1
  (token1 : tokens) -> do
    s1 <- parseToken token1
    spaces <- case (s1, token1) of
      ([], Optional _) -> pure []
      _ -> commentOrSpaceParser True <|> pure []

    others <- spaceSeparatedTokensToParser tokens
    pure $ s1 <> spaces <> others
  where
    parseToken = \case
      Optional t -> spaceSeparatedTokensToParser t <|> pure []
      CITextToken t -> (\pt -> [Block pt]) <$> asciiCI t
      CustomParserToken p -> p
      SqlIdentifier -> objIdentifier
      CommaSeparatedIdentifiers -> listOfAtLeast1 [SqlIdentifier] ","
      AllUntilEndOfStatement -> do
        t1 <-
          NotBlock
            <$> takeWhile
              (\c -> not (isPossibleBlockStartingChar c) && c /= ';')
        mc <- peekChar
        case mc of
          Nothing -> pure [t1]
          Just ';' -> do
            void $ char ';'
            pure [t1, NotBlock ";"]
          Just _ -> do
            t2 <- many1 blockParser <|> (\pt -> [[NotBlock pt]]) <$> Parsec.take 1
            -- After reading blocks or just a char, we still need to find a semi-colon to get a statement from start to finish!
            t3 <- parseToken AllUntilEndOfStatement
            pure $ t1 : mconcat t2 <> t3

listOfAtLeast1 :: [SqlToken] -> Text -> Parser [BlockOrNotBlock]
listOfAtLeast1 elementTokens separator = do
  firstEl <- spaceSeparatedTokensToParser elementTokens
  otherEls <-
    mconcat
      <$> many'
        ( spaceSeparatedTokensToParser $
            CustomParserToken (pure [])
              : CITextToken separator
              : elementTokens
        )
  pure $ firstEl <> otherEls

-- Urgh.. parsing statements precisely would benefit a lot from importing the lex parser
copyFromStdinStatementParser :: Parser SqlPiece
copyFromStdinStatementParser = do
  stmt <-
    spaceSeparatedTokensToParser
      [ CITextToken "COPY",
        SqlIdentifier,
        Optional
          [ CITextToken "(",
            Optional [CommaSeparatedIdentifiers],
            CITextToken ")"
          ],
        CITextToken "FROM",
        CITextToken "STDIN",
        AllUntilEndOfStatement
      ]
  seol <- eol
  pure $ CopyFromStdinStatement $ blockListText stmt <> seol

-- | Parser to be used after "COPY FROM STDIN..." has been parsed with `copyFromStdinStatementParser`.
copyFromStdinAfterStatementParser :: Int -> Parser ([SqlPiece], ParserState)
copyFromStdinAfterStatementParser approxMaxChunkSize = do
  when (approxMaxChunkSize <= 0) $
    error "approxMaxChunkSize must be strictly positive"
  -- This stateful parser is tricky to get right but it's proven to be much faster than simpler
  -- alternatives I've tried (e.g. taking lines and concatenating them was incredibly slow for some reason)
  (contents, (_, _, terminatorLen)) <-
    Parsec.runScanner
      (0 :: Int, 0 :: Int, 0 :: Int)
      ( \(lenTotalParsed, lenCurrentLine, lenTerminatorSoFar) c ->
          if lenTotalParsed >= approxMaxChunkSize && lenCurrentLine == 0
            then Nothing -- Only stop at the beginning of a new line
            else
              if lenCurrentLine /= lenTerminatorSoFar && c == '\n'
                then Just (1 + lenTotalParsed, 0, 0)
                else
                  if lenCurrentLine /= lenTerminatorSoFar
                    then Just (1 + lenTotalParsed, 1 + lenCurrentLine, 0)
                    else case (lenTerminatorSoFar, c) of
                      (0, '\\') ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 1)
                      (1, '.') ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 2)
                      (2, '\n') ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 3) -- Last char in terminator, but `Just` because it needs to be in the parsed contents
                      (3, _) -> Nothing -- Terminator with len=3 means it's been parsed, so end here.
                      (_, '\n') -> Just (1 + lenTotalParsed, 0, 0)
                      _ ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 0)
      )
  isEnd :: Bool <- Parsec.atEnd
  let fullTerminator = "\\.\n"
      eofTerminator = "\\."
      terminatorFound
        | terminatorLen == 3 = fullTerminator
        | isEnd && terminatorLen == 2 = eofTerminator
        | otherwise = ""
      rows = Text.dropEnd terminatorLen contents
  case (rows, terminatorFound) of
    ("", "") -> pure ([], InsideCopy) -- This should be impossible
    ("", _) -> pure ([CopyFromStdinEnd terminatorFound], OutsideCopy)
    (_, "") -> pure ([CopyFromStdinRows rows], InsideCopy)
    (_, _) ->
      pure
        ( [CopyFromStdinRows rows, CopyFromStdinEnd terminatorFound],
          OutsideCopy
        )

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser [BlockOrNotBlock]
commentOrSpaceParser atLeastOne =
  if atLeastOne
    then many1 (commentParser <|> Block <$> takeWhile1 Char.isSpace)
    else many' (commentParser <|> Block <$> takeWhile1 Char.isSpace)

commentParser :: Parser BlockOrNotBlock
commentParser = doubleDashComment <|> cStyleComment

eol :: Parser Text
eol = string "\n" <|> string "\r\n"

blockText :: BlockOrNotBlock -> Text
blockText = \case
  Block t -> t
  NotBlock t -> t

-- TODO: Doesn't attoparsec have something that returns all of the parsed text when applying
-- a Parser? We should probably use that instead of this.
blockListText :: [BlockOrNotBlock] -> Text
blockListText = Text.concat . map blockText

flattenBlocks :: [BlockOrNotBlock] -> [BlockOrNotBlock]
flattenBlocks =
  map
    ( \bs@(firstEl :| _) -> case firstEl of
        NotBlock _ -> NotBlock $ blockListText $ NE.toList bs
        Block _ -> Block $ blockListText $ NE.toList bs
    )
    . NE.groupBy
      ( \a b -> case (a, b) of
          (NotBlock _, NotBlock _) -> True
          (Block _, Block _) -> True
          _ -> False
      )
    . filter (\b -> blockText b /= "")

flattenBlocksInPieces :: [SqlPiece] -> [SqlPiece]
flattenBlocksInPieces =
  map
    ( \case
        OtherSqlPiece bs -> OtherSqlPiece $ flattenBlocks bs
        ctor -> ctor
    )

mapBlock :: (Text -> Text) -> BlockOrNotBlock -> BlockOrNotBlock
mapBlock f = \case
  Block t -> Block $ f t
  NotBlock t -> NotBlock $ f t

-- For now, this assumes standard_conforming_strings is always on.
blockParser :: Parser [BlockOrNotBlock]
blockParser =
  -- Unicode escaped strings aren't explicitly implemented, but work with the current parser.
  -- Since single quotes can't be an escape character for them (see https://www.postgresql.org/docs/current/sql-syntax-lexical.html),
  -- backslashes will be treated like a regular character - which works for us -, and if other characters are chosen as the escape character,
  -- our parsers will treat those also as regular characters, which should be fine.
  -- This seems fragile, but our tests will error out if changes make this unsupported.
  (: []) <$> parseStdConformingString
    <|> parenthesisedExpression
    <|> (: []) <$> cStyleComment
    <|> (: []) <$> dollarStringParser
    <|> (: []) <$> doubleDashComment
    <|> (: []) <$> doubleQuotedIdentifier
    <|> (: []) <$> cStyleEscapedString

-- | A character that may be the first of a block. This needs to match parsers in `blockParser`, and is only useful
-- to optimize our parsers by avoiding backtracking through usage of `takeWhile` and similar.
isPossibleBlockStartingChar :: Char -> Bool
isPossibleBlockStartingChar c =
  c
    == '('
    || c
      == '-'
    || c
      == '/'
    || c
      == '"'
    || c
      == '$'
    || c
      == '\''
    || c
      == 'E'

dollarStringParser :: Parser BlockOrNotBlock
dollarStringParser = fmap Block $ do
  void $ char '$'
  b <- takeWhile (/= '$')
  void $ char '$'
  let dollarSep = "$" <> b <> "$"
  rest <- go dollarSep
  pure $ dollarSep <> rest
  where
    go dollarSep = do
      t <- takeWhile (/= '$')
      ending <- optional $ string dollarSep <|> "" <$ endOfInput
      case ending of
        Nothing -> do
          void $ char '$'
          rest <- go dollarSep
          pure $ t <> "$" <> rest
        Just e -> pure $ t <> e

doubleDashComment :: Parser BlockOrNotBlock
doubleDashComment = fmap Block $ do
  begin <- string "--"
  rest <- Parsec.takeWhile (\c -> c /= '\n' && c /= '\r')
  end <- eol <|> "" <$ endOfInput
  pure $ begin <> rest <> end

cStyleComment :: Parser BlockOrNotBlock
cStyleComment = fmap Block $ do
  openComment <- string "/*"
  rest <-
    Parsec.scan
      (1 :: Int, False, False)
      ( \(openCommentCount, hasPartialOpening, hasPartialClosing) c ->
          nothingWhenDone $ case (hasPartialOpening, hasPartialClosing, c) of
            -- Handle slashes in all possible contexts
            (False, False, '/') -> Just (openCommentCount, True, False)
            (False, True, '/') -> Just (openCommentCount - 1, False, False)
            (True, False, '/') -> Just (openCommentCount, True, False)
            -- Handle asterisks in all possible contexts
            (False, False, '*') -> Just (openCommentCount, False, True)
            (True, False, '*') -> Just (openCommentCount + 1, False, False)
            (False, True, '*') -> Just (openCommentCount, False, True)
            -- Handle other characters
            (True, True, _) ->
              error
                "Report this as a bug in codd: C Style comment parser invalid state"
            _ -> Just (openCommentCount, False, False)
      )
  end <- string "/" <|> "" <$ endOfInput -- We are generous with eof and allow even invalid SQL in many places
  pure $ openComment <> rest <> end
  where
    nothingWhenDone (Just (0, _, _)) = Nothing
    nothingWhenDone x = x

parenthesisedExpression :: Parser [BlockOrNotBlock]
parenthesisedExpression = do
  openParen <- string "(" <|> fail "No open paren"
  rest <- insideParenParser
  pure $ NotBlock openParen : rest
  where
    insideParenParser :: Parser [BlockOrNotBlock]
    insideParenParser = do
      more <-
        takeWhile
          (\c -> not (isPossibleBlockStartingChar c) && c /= ')')
      nextChar <- peekChar
      case nextChar of
        Nothing -> pure [NotBlock more] -- Be gentle with EOF
        Just ')' -> do
          closeParen <- string ")"
          pure [NotBlock more, NotBlock closeParen]
        Just _ -> do
          blocksOrOtherwise <- blockParser <|> (\t -> [NotBlock t]) <$> Parsec.take 1
          rest <- insideParenParser -- We're still inside an openParen after parsing a block or a character
          pure $ NotBlock more : blocksOrOtherwise ++ rest

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Does not consume the ending char and RETURNS any
-- backslash escape chars in the result.
-- Use `parseWithEscapeCharProper` to exclude the escape chars from the result.
-- This function is useful if you want the original parsed contents.
parseWithEscapeCharPreserve :: (Char -> Bool) -> Parser Text
parseWithEscapeCharPreserve untilc = do
  cs <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
  nextChar <- peekChar
  case nextChar of
    Just '\\' -> do
      c <- Parsec.take 2
      rest <- parseWithEscapeCharPreserve untilc
      pure $ cs <> c <> rest
    _ -> pure cs

-- | Identifiers can be in fully qualified form `"database"."schema"."objectname"`,
-- with and without double quoting, e.g.: `"schema".tablename`, or just a simple `tablename`.
objIdentifier :: Parser [BlockOrNotBlock]
objIdentifier =
  let singleIdentifier =
        fmap (: []) $
          doubleQuotedIdentifier
            <|> Block
              <$> takeWhile1
                ( \c ->
                    not (Char.isSpace c)
                      && c
                        /= ','
                      && c
                        /= '.'
                      && c
                        /= ')'
                ) -- TODO: What are the valid chars for identifiers?? Figure it out!!
   in listOfAtLeast1 [CustomParserToken singleIdentifier] "."

doubleQuotedIdentifier :: Parser BlockOrNotBlock
doubleQuotedIdentifier = fmap Block $ do
  openingQuote <- string "\""
  rest <- parseWithEscapeCharPreserve (== '"')
  ending <- string "\""
  pure $ openingQuote <> rest <> ending

-- | Parses a single quoted NON standard conforming string, i.e. strings that use backslash as an escape character, and are marked
-- by beginning with an `E`. Consecutive simple quotes are also treated as a single quote, just like in std conforming strings.
-- See https://www.postgresql.org/docs/current/sql-syntax-lexical.html
cStyleEscapedString :: Parser BlockOrNotBlock
cStyleEscapedString = fmap Block $ do
  openingChars <- string "E'"
  rest <-
    Parsec.scan
      (False, False)
      ( \(lastCharWasBackslash, lastCharWasSingleQuote) c ->
          case ((lastCharWasBackslash, lastCharWasSingleQuote), c) of
            ((False, False), '\'') -> Just (False, True)
            ((False, True), '\'') -> Just (False, False) -- Two consecutive single quotes are not end of string
            ((False, True), _) -> Nothing -- A single quote not preceded by \ and followed by not-a-single-quote is end of string
            ((False, False), '\\') -> Just (True, False)
            ((False, False), _) -> Just (False, False) -- "regular" character
            ((True, False), _) -> Just (False, False) -- Any character after a backslash must be included in the string
            ((True, True), _) ->
              error
                "Please submit this as a bug report to codd, saying both backslash and single quote were last char in cStyleEscapedString"
      )
  pure $ openingChars <> rest

-- | Parses a single quoted standard conforming string, i.e. strings that use '' as a representation of a single quote, and
-- takes any other character literally.
parseStdConformingString :: Parser BlockOrNotBlock
parseStdConformingString = fmap Block $ do
  openingQuote <- string "'"
  rest <-
    Parsec.scan
      False
      ( \lastCharWasQuote c -> case (lastCharWasQuote, c) of
          (False, '\'') -> Just True
          (True, '\'') -> Just False -- Two consecutive single quotes represent a single quote, not end of string
          (True, _) -> Nothing -- One single quote followed by any other character means that single quote was end of string
          (False, _) -> Just False
      )
  pure $ openingQuote <> rest

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

-- | Parser that consumes only the space character, not other kinds of white space.
skipJustSpace :: Parser ()
skipJustSpace = skipWhile (== ' ')

-- | Parser that consumes any kind of Unicode space character, including \t, \n, \r, \f, \v.
skipAllWhiteSpace :: Parser ()
skipAllWhiteSpace = skipWhile Char.isSpace

isWhiteSpacePiece :: SqlPiece -> Bool
isWhiteSpacePiece (WhiteSpacePiece _) = True
isWhiteSpacePiece _ = False

isCommentPiece :: SqlPiece -> Bool
isCommentPiece (CommentPiece _) = True
isCommentPiece _ = False

piecesToText :: (Foldable t) => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""
