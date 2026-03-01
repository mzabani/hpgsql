{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ParsingSpec where

import Control.Monad
  ( forM,
    forM_,
    forever,
    guard,
    unless,
    void,
    when,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import GHC.Num (Natural)
import HPgsql.Parsing (BlockOrNotBlock (..), SqlStatement (..), flattenBlocksInPieces, parseSql, sqlStatementText)
import HPgsql.Query (Query (..), SingleQuery (..), sql)
import Hedgehog (Gen, forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Streaming (Of (..), Stream)
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC, performMajorGC)
import System.Random (mkStdGen, randomR)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

newtype RandomSql = RandomSql
  { unRandomSql :: Text
  }
  deriving newtype (Show)

-- | Syntactically valid SQL must contain at least one statement!
newtype SyntacticallyValidRandomSql = SyntacticallyValidRandomSql
  { unSyntRandomSql :: Text
  }
  deriving newtype (Show)

genSingleSqlStatement :: Gen SqlStatement
genSingleSqlStatement = Gen.element validSqlStatements

-- | Sql statements that can be interleaved in any form and should still form syntactically valid SQL.
validSqlStatements :: [SqlStatement]
validSqlStatements =
  [ SqlStatement [NotBlock "SELECT ", Block "'so''m -- not a comment'", NotBlock " FROM ahahaha;"],
    SqlStatement
      [NotBlock "SELECT\n", Block "E'so\\'; \\; m -- c-style string, (( not a comment \\\\ \\abc'", NotBlock " FROM ahahaha;"],
    SqlStatement
      [NotBlock "SELECT \n\n  ", Block "E'Consecutive single quotes are also escaped here ''; see?'", NotBlock "   \n ;"],
    SqlStatement [NotBlock "SELECT ", Block "E'''20s'", NotBlock " = ", Block "E'\\'20s'", NotBlock ";"],
    SqlStatement
      [ NotBlock "DO\n",
        Block $
          "$do$"
            <> "\nBEGIN"
            <> "\n   IF NOT EXISTS ("
            <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
            <> "\n"
            <> "\n      CREATE USER \"codd-user\";"
            <> "\n   END IF;"
            <> "\nEND"
            <> "\n$do$",
        NotBlock ";"
      ],
    SqlStatement [NotBlock "CREATE TABLE ", Block "\"escaped--table /* nasty */\"", NotBlock ";"],
    SqlStatement [NotBlock "SELECT x, pg_sleep(x / 1000.0) FROM generate_series(1,1000) q(x);"],
    SqlStatement [NotBlock "CREATE TABLE any_table();"],
    SqlStatement
      [ NotBlock $
          "CREATE FUNCTION sales_tax(subtotal real) RETURNS real AS ",
        Block
          ( "$$"
              <> "\nBEGIN"
              <> "\n    RETURN subtotal * 0.06;"
              <> "\nEND;"
              <> "\n$$"
          ),
        NotBlock " LANGUAGE plpgsql;"
      ],
    SqlStatement
      [ NotBlock
          "CREATE FUNCTION instr(varchar, integer) RETURNS integer AS ",
        Block $
          "$$"
            <> "\nDECLARE"
            <> "\n    v_string ALIAS FOR $1;"
            <> "\n    index ALIAS FOR $2;"
            <> "\nBEGIN"
            <> "\n    -- some computations using v_string and index here"
            <> "\nEND;"
            <> "\n$$",
        NotBlock
          " LANGUAGE plpgsql;"
      ],
    SqlStatement [NotBlock "select U&", Block "'d\\0061t\\+000061'", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "'\\0441\\043B\\043E\\043D'", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "'d!0061t!+000061'", NotBlock " UESCAPE ", Block "'!'", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "'d\\0061t\\+000061'", NotBlock " UESCAPE ", Block "'\\'", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "\"d\\0061t\\+000061\"", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "\"\\0441\\043B\\043E\\043D\"", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "\"d!0061t!+000061\"", NotBlock " UESCAPE ", Block "'!'", NotBlock ";"],
    SqlStatement [NotBlock "select U&", Block "\"d\\0061t\\+000061\"", NotBlock " UESCAPE ", Block "'\\'", NotBlock ";"],
    SqlStatement [NotBlock "select X", Block "'1FF'", NotBlock ";"],
    SqlStatement [NotBlock "select B", Block "'1001'", NotBlock ";"],
    SqlStatement [NotBlock "SELECT ", Block "'some''quoted ''string'", NotBlock ";"],
    SqlStatement [NotBlock "SELECT ", Block "\"some\"\"quoted identifier\"", NotBlock ";"],
    SqlStatement
      [NotBlock "SELECT \n\n ", Block "'double quotes \" inside single quotes \" - 2'", NotBlock ";"],
    SqlStatement
      [NotBlock "SELECT ", Block "\"single quotes ' inside double quotes ' - 2\"", NotBlock ";"],
    SqlStatement
      [ Block $
          "$function$"
            <> "\nBEGIN"
            <> "\n    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$); /* Some would-be non terminated comment, but it's fine inside dollar quotes"
            <> "\nEND;"
            <> "\n$function$",
        NotBlock ";"
      ],
    SqlStatement [NotBlock "SELECT COALESCE(4, 1 - 2) - 3 + 4 - 5;"],
    SqlStatement [NotBlock "SELECT (1 - 4) / 5 * 3 / 9.1;"],
    -- Semi-colons inside parenthesised blocks are not statement boundaries
    SqlStatement
      [NotBlock "create rule \n name as on \n some_event to \n\n some_table do (command1; command2; command3;);"],
    -- String blocks can be opened inside parentheses, just like comments, dollar-quoted strings and others
    SqlStatement
      [NotBlock "create rule name as on some_event to some_table do (", Block "'abcdef);'", NotBlock "; other;)\n\n;"],
    SqlStatement
      [NotBlock "some statement with spaces (((U&", Block "'d\\0061t\\+000061'", NotBlock ", ", Block "'abc''def'", NotBlock " ; ; ; ", Block "/* comment /* nested */ ((( */", NotBlock " ", Block "$hey$dollar string$hey$", NotBlock ")) ", Block "'more''of'", NotBlock "; this; ());"],
    SqlStatement
      [NotBlock "select ", Block "'unclosed parentheses inside string ((((('", NotBlock ", ((", Block "'string (('", NotBlock "));"],
    SqlStatement
      [ NotBlock $
          "CREATE STATISTICS IF NOT EXISTS test_stat_expr"
            <> "\n( dependencies,mcv) ON employee_id, lower(employee_name)"
            <> "\nFROM employee;"
      ],
    -- We still want the following to be parsed; it's best to run invalid statements than have
    -- a parser so strict that it might refuse valid ones.
    SqlStatement [NotBlock "invalid statement, bad parentheses ()));"],
    SqlStatement [NotBlock "begin;"],
    SqlStatement [NotBlock "BEGiN", Block "/*a*/", NotBlock ";"],
    SqlStatement [NotBlock "BEgIN \n  ;"],
    SqlStatement [NotBlock "ROllBaCk;"],
    SqlStatement [NotBlock "ROllBaCk", Block "/*a*/", NotBlock ";"],
    SqlStatement [NotBlock "ROllBaCk   ;"],
    SqlStatement [NotBlock "COmmIT;"],
    SqlStatement [NotBlock "COMMIT", Block "/*a*/", NotBlock ";"],
    SqlStatement [NotBlock "cOMMIT   ;"],
    SqlStatement
      [NotBlock "COPY employee FROM STDIN WITH (FORMAT CSV);"],
    SqlStatement
      [NotBlock "copy ", Block "\"]schema\"", NotBlock ".employee FROM stdin \n WITH (FORMAT CSV);"],
    -- Fully qualified identifiers part 1 + table without columns, but with one row (this is possible!)
    SqlStatement
      [NotBlock "CoPy ", Block "\"]some-database\"", NotBlock "   .  ", Block "\"schema\"", NotBlock "  .  employee from stdin with \n (FORMAT CSV);"],
    -- Fully qualified identifiers part 2 + specifying columns
    SqlStatement
      [NotBlock "CoPy ", Block "\"]employee\"", NotBlock " \n  (col1,", Block "\"col2\"", NotBlock "   , \n\n  col4  ) from stdin with (FORMAT CSV);"]
  ]

newtype ShuffleOfPieces = ShuffleOfPieces [SqlStatement] deriving stock (Show)

genShuffleOfPieces :: Gen ShuffleOfPieces
genShuffleOfPieces = ShuffleOfPieces <$> Gen.shuffle validSqlStatements

genSql :: Bool -> Gen Text
genSql onlySyntacticallyValid =
  if onlySyntacticallyValid
    then randomSqlGen
    else Gen.frequency [(1, pure ""), (50, randomSqlGen)]
  where
    emptyLineGen = pure "\n"
    bizarreLineGen = (<> "\n") <$> Gen.text (Range.linear 0 100) Gen.unicode
    lineGen =
      if onlySyntacticallyValid
        then
          Gen.frequency
            [ (1, emptyLineGen),
              (5, sqlStatementText <$> genSingleSqlStatement)
            ]
        else
          Gen.frequency
            [ (1, bizarreLineGen),
              (1, emptyLineGen),
              (5, sqlStatementText <$> genSingleSqlStatement)
            ]
    -- Note: the likelihood that Hedgehog will randomly generate text that has a line starting with "-- codd:"
    -- is so low that we can just ignore it
    dashDashCommentGen =
      (<> "\n") . ("-- " <>) . Text.replace "\n" "" <$> bizarreLineGen
    cStyleCommentGen =
      ("/*" <>) . (<> "*/") . Text.replace "*/" "" <$> bizarreLineGen
    commentGen = Gen.frequency [(5, dashDashCommentGen), (1, cStyleCommentGen)]
    lineOrCommentGen = Gen.frequency [(5, lineGen), (1, commentGen)]
    randomSqlGen = fmap Text.concat $ do
      l1 <- Gen.list (Range.linear 0 100) lineOrCommentGen
      l2 <- Gen.list (Range.linear 0 100) lineOrCommentGen
      atLeastOneStmt <- sqlStatementText <$> genSingleSqlStatement
      let finalList =
            if onlySyntacticallyValid
              then l1 ++ (atLeastOneStmt : l2)
              else l1 ++ l2

          mapLast :: (a -> a) -> [a] -> [a]
          mapLast _ [] = []
          mapLast f [x] = [f x]
          mapLast f (x : xs) = x : mapLast f xs

      -- Optionally remove semi-colon from the last command if it ends with one
      removeLastSemiColon <- Gen.bool
      pure $
        if removeLastSemiColon
          then mapLast (\t -> fromMaybe t (Text.stripSuffix ";" t)) finalList
          else finalList

genRandomSql :: Gen RandomSql
genRandomSql = RandomSql <$> genSql False

genSyntacticallyValidRandomSql :: Gen SyntacticallyValidRandomSql
genSyntacticallyValidRandomSql = SyntacticallyValidRandomSql <$> genSql True

-- | Same as a monadic `shouldSatisfy` isLeft, but does not require a Show instance.
shouldReturnLeft :: (MonadIO m) => m (Either a b) -> m ()
shouldReturnLeft mv =
  mv >>= \case
    Left _ -> pure ()
    Right _ -> liftIO $ expectationFailure "Got Right but was expecting Left"

spec :: Spec
spec = do
  describe "Parsing tests" $ do
    context "Simple query parsing tests" $ do
      it "parses SQL without interpolation" $ do
        let Query queries = [sql|SELECT * FROM users|]
            query = getUniqueNE queries
        queryString query `shouldBe` encodeUtf8 "SELECT * FROM users"
        queryParams query `shouldBe` []

      it "parses SQL with single interpolation" $ do
        let userId = 42 :: Int
            Query queries = [sql|SELECT * FROM users WHERE id = #{userId}|]
            query = getUniqueNE queries
        queryString query `shouldBe` encodeUtf8 "SELECT * FROM users WHERE id = $1"
        length (queryParams query) `shouldBe` 1

      it "parses SQL with multiple interpolations" $ do
        let userId = 42 :: Int
            userName = "john" :: Text
            Query queries = [sql|SELECT * FROM users WHERE id = #{userId} AND name = #{userName}|]
            query = getUniqueNE queries
        queryString query `shouldBe` encodeUtf8 "SELECT * FROM users WHERE id = $1 AND name = $2"
        length (queryParams query) `shouldBe` 2
    it "Single command with and without semi-colon" $ hedgehog $ do
      randomSeed <- forAll $ Gen.int Range.linearBounded
      liftIO $ do
        let stmts :: [NonEmpty SqlStatement] =
              map
                parseSql
                [ "CREATE TABLE hello;",
                  "CREATE TABLE hello",
                  "CREATE TABLE hello; -- Comment",
                  "CREATE TABLE hello -- Comment",
                  "CREATE TABLE hello -- Comment\n;"
                ]
        map (fmap flattenBlocksInPieces) stmts
          `shouldBe` [ NE.singleton $ SqlStatement [NotBlock "CREATE TABLE hello;"],
                       NE.singleton $ SqlStatement [NotBlock "CREATE TABLE hello"],
                       NE.singleton $
                         SqlStatement
                           [ NotBlock "CREATE TABLE hello;",
                             Block " -- Comment"
                           ],
                       NE.singleton $
                         SqlStatement
                           -- The comment below not being a CommentPiece is definitely a limitation
                           -- of our parser. But it still satisfies two important criteria:
                           -- 1. The comment gets sent with the query, which is important for identification
                           --    in logs.
                           -- 2. Question marks/parameter bindings inside the comment will not be replaced
                           [NotBlock "CREATE TABLE hello ", Block "-- Comment"],
                       NE.singleton $
                         SqlStatement
                           [NotBlock "CREATE TABLE hello ", Block "-- Comment\n", NotBlock ";"]
                     ]
    it "Statement separation boundaries are good" $ hedgehog $ do
      origPieces <- fmap NE.fromList $ forAll $ Gen.list (Range.linear 1 100) genSingleSqlStatement
      let allSqlAsOneBigText :: Text =
            mconcat $
              map sqlStatementText $
                NE.toList origPieces
      liftIO $ do
        let parsedPieces = parseSql allSqlAsOneBigText
        fmap flattenBlocksInPieces parsedPieces
          `shouldBe` fmap flattenBlocksInPieces origPieces
    it "Statements concatenation matches original" $ hedgehog $ do
      SyntacticallyValidRandomSql {..} <- forAll genSyntacticallyValidRandomSql
      liftIO $ do
        let stmts = parseSql unSyntRandomSql
        mconcat (map sqlStatementText (NE.toList stmts)) `shouldBe` Text.strip unSyntRandomSql

    it "Sql Migration Parser never fails, even for random text" $ hedgehog $ do
      RandomSql {unRandomSql} <- forAll genRandomSql
      liftIO $ do
        let stmts = parseSql unRandomSql
        let t = mconcat $ map sqlStatementText $ NE.toList stmts
        t `shouldBe` Text.strip unRandomSql

getUniqueNE :: (HasCallStack) => NonEmpty a -> a
getUniqueNE l
  | length l /= 1 = error "NonEmpty list expected to have a single element has more than one"
  | otherwise = NE.head l
