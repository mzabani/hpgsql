-- |
--
-- This module contains parsers that are helpful to separate SQL statements from each other by finding query boundaries: semi-colons, but not when inside a string or a parenthesised expression, for example.
module HPgsql.Parsing
  ( parseSql,
    flattenBlocksInPieces,
    sqlStatementText,
    SqlStatement (..),
    BlockOrNotBlock (..),
  )
where

import Control.Applicative
  ( optional,
    (<|>),
  )
import Control.Monad
  ( void,
  )
import Data.Attoparsec.Text
  ( Parser,
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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (takeWhile)

data SqlStatement = SqlStatement ![BlockOrNotBlock]
  deriving stock (Show, Eq)

-- | Blocks are the name we give to some expressions that have a beginning and an end, inside of which
-- semicolons are not to be considered statement boundaries. These include strings, comments,
-- parenthesised expressions and dollar-quoted strings.
-- Knowing if a fragment of SQL is a block or not can help interpolate '?' or other placeholder
-- strings as arguments in some contexts, while avoiding doing so inside e.g. text strings.
data BlockOrNotBlock = Block !Text | NotBlock !Text
  deriving stock (Eq, Show)

-- | Parses one or more SQL statements (separated by semi-colons).
parseSql :: Text -> NonEmpty SqlStatement
parseSql (Text.strip -> str) = case Parsec.parseOnly (many' sqlStatementParser <* endOfInput) str of
  Right mStatements ->
    case NE.nonEmpty mStatements of
      Just stmts -> fmap SqlStatement stmts
      Nothing -> if str == "" then NE.singleton $ SqlStatement [] else error "Bug in hpgsql when parsing SQL. No statements found."
  Left err -> error $ "Bug in hpgsql when parsing SQL: " ++ err

sqlStatementText :: SqlStatement -> Text
sqlStatementText (SqlStatement ls) = Text.concat $ map blockText ls

sqlStatementParser :: Parser [BlockOrNotBlock]
sqlStatementParser = do
  t1 <-
    takeWhile
      (\c -> not (isPossibleBlockStartingChar c) && c /= ';')
  mc <- peekChar
  case mc of
    Nothing -> if t1 == "" then fail "Cannot parse empty string as SQL statement" else pure [NotBlock t1]
    Just ';' -> do
      void $ char ';'
      -- We take any comments followed by EOF here
      cws <- optional $ commentOrSpaceParser True <* endOfInput
      pure $ [NotBlock t1, NotBlock ";"] ++ fromMaybe [] cws
    Just _ -> do
      t2 <- many1 blockParser <|> (\pt -> [[NotBlock pt]]) <$> Parsec.take 1
      -- After reading blocks or just a char, we still need to find a semi-colon
      -- or EOF to get a statement from start to finish!
      t3 <- sqlStatementParser <|> ([] <$ endOfInput)
      pure $ NotBlock t1 : mconcat t2 <> t3

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser [BlockOrNotBlock]
commentOrSpaceParser atLeastOne =
  if atLeastOne
    then many1 (commentParser <|> Block <$> takeWhile1 Char.isSpace)
    else many' (commentParser <|> Block <$> takeWhile1 Char.isSpace)
  where
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

flattenBlocksInPieces :: SqlStatement -> SqlStatement
flattenBlocksInPieces (SqlStatement blks) =
  SqlStatement $ flattenBlocks blks
