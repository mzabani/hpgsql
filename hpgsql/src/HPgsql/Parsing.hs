-- |
--
-- This module contains parsers that are helpful to separate SQL statements from each other by finding query boundaries: semi-colons, but not when inside a string or a parenthesised expression, for example.
module HPgsql.Parsing
  ( parseSql,
    piecesToText,
    SqlPiece (..),
    BlockOrNotBlock (..),
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
    string,
    takeWhile,
    takeWhile1,
  )
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (takeWhile)

data SqlPiece = CommentPiece !Text | WhiteSpacePiece !Text | OtherSqlPiece ![BlockOrNotBlock]
  deriving stock (Show, Eq)

-- | Blocks are the name we give to some expressions that have a beginning and an end, inside of which
-- semicolons are not to be considered statement boundaries. These include strings, comments,
-- parenthesised expressions and dollar-quoted strings.
-- Knowing if a fragment of SQL is a block or not can help interpolate '?' or other placeholder
-- strings as arguments in some contexts, while avoiding doing so inside e.g. text strings.
data BlockOrNotBlock = Block !Text | NotBlock !Text
  deriving stock (Eq, Show)

parseSql :: String -> [SqlPiece]
parseSql str =
  case Parsec.parseOnly (many' sqlPieceParser <* endOfInput) (Text.pack str) of
    Right pieces -> pieces
    Left err -> error $ "Please report this as a bug in hpgsql: " ++ err

sqlPieceText :: SqlPiece -> Text
sqlPieceText (CommentPiece s) = s
sqlPieceText (WhiteSpacePiece s) = s
sqlPieceText (OtherSqlPiece ls) = Text.concat $ map blockText ls

sqlPieceParser :: Parser SqlPiece
sqlPieceParser =
  CommentPiece
    . blockText
    <$> commentParser
    <|> WhiteSpacePiece
      <$> takeWhile1
        (\c -> Char.isSpace c || c == '\n' || c == '\r' || c == '\t')
    <|> OtherSqlPiece
      <$> anySqlPieceParser
  where
    anySqlPieceParser = do
      arbText <- spaceSeparatedTokensToParser [AllUntilEndOfStatement]
      when (blockListText arbText == "") $ fail "Please report this as a bug in hpgsql: trying to parse empty string as SQL piece"
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

piecesToText :: (Foldable t) => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""
