{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ParsingSpec where

import Control.Exception (SomeException, evaluate, try)
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
import Debug.Trace
import GHC.Num (Natural)
import Hedgehog (Gen, annotateShow, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hpgsql (Only (..))
import Hpgsql.Encoding (ToPgRow (..))
import Hpgsql.Parsing (BlockOrNotBlock (..), ParsingOpts (..), QQExprKind (..), blockListText, flattenBlocks, parseSql)
import Hpgsql.InternalTypes (Query (..), SingleQuery (..))
import Hpgsql.Query (breakQueryIntoStatements, mkQuery, sql)
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

genSingleSqlStatement :: Gen [BlockOrNotBlock]
genSingleSqlStatement = Gen.element validSqlStatements

-- | Sql statements that can be interleaved in any form and should still form syntactically valid SQL.
validSqlStatements :: [[BlockOrNotBlock]]
validSqlStatements =
  [ [StaticSql "SELECT ", StaticSql "'so''m -- not a comment'", StaticSql " FROM ahahaha;"],
    [StaticSql "SELECT\n", StaticSql "E'so\\'; \\; m -- c-style string, (( not a comment \\\\ \\abc'", StaticSql " FROM ahahaha;"],
    [StaticSql "SELECT \n\n  ", StaticSql "E'Consecutive single quotes are also escaped here ''; see?'", StaticSql "   \n ;"],
    [StaticSql "SELECT ", StaticSql "E'''20s'", StaticSql " = ", StaticSql "E'\\'20s'", SemiColon],
    [ StaticSql "DO\n",
      StaticSql $
        "$do$"
          <> "\nBEGIN"
          <> "\n   IF NOT EXISTS ("
          <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
          <> "\n"
          <> "\n      CREATE USER \"codd-user\";"
          <> "\n   END IF;"
          <> "\nEND"
          <> "\n$do$",
      SemiColon
    ],
    [StaticSql "CREATE TABLE ", StaticSql "\"escaped--table /* nasty */\"", SemiColon],
    [StaticSql "SELECT x, pg_sleep(x / 1000.0) FROM generate_series(1,1000) q(x);"],
    [StaticSql "CREATE TABLE any_table();"],
    [ StaticSql $
        "CREATE FUNCTION sales_tax(subtotal real) RETURNS real AS ",
      StaticSql
        ( "$$"
            <> "\nBEGIN"
            <> "\n    RETURN subtotal * 0.06;"
            <> "\nEND;"
            <> "\n$$"
        ),
      StaticSql " LANGUAGE plpgsql;"
    ],
    [ StaticSql
        "CREATE FUNCTION instr(varchar, integer) RETURNS integer AS ",
      StaticSql $
        "$$"
          <> "\nDECLARE"
          <> "\n    v_string ALIAS FOR $1;"
          <> "\n    index ALIAS FOR $2;"
          <> "\nBEGIN"
          <> "\n    -- some computations using v_string and index here"
          <> "\nEND;"
          <> "\n$$",
      StaticSql
        " LANGUAGE plpgsql;"
    ],
    [StaticSql "select U&", StaticSql "'d\\0061t\\+000061'", SemiColon],
    [StaticSql "select U&", StaticSql "'\\0441\\043B\\043E\\043D'", SemiColon],
    [StaticSql "select U&", StaticSql "'d!0061t!+000061'", StaticSql " UESCAPE ", StaticSql "'!'", SemiColon],
    [StaticSql "select U&", StaticSql "'d\\0061t\\+000061'", StaticSql " UESCAPE ", StaticSql "'\\'", SemiColon],
    [StaticSql "select U&", StaticSql "\"d\\0061t\\+000061\"", SemiColon],
    [StaticSql "select U&", StaticSql "\"\\0441\\043B\\043E\\043D\"", SemiColon],
    [StaticSql "select U&", StaticSql "\"d!0061t!+000061\"", StaticSql " UESCAPE ", StaticSql "'!'", SemiColon],
    [StaticSql "select U&", StaticSql "\"d\\0061t\\+000061\"", StaticSql " UESCAPE ", StaticSql "'\\'", SemiColon],
    [StaticSql "select X", StaticSql "'1FF'", SemiColon],
    [StaticSql "select B", StaticSql "'1001'", SemiColon],
    [StaticSql "SELECT ", StaticSql "'some''quoted ''string'", SemiColon],
    [StaticSql "SELECT ", StaticSql "\"some\"\"quoted identifier\"", SemiColon],
    [StaticSql "SELECT \n\n ", StaticSql "'double quotes \" inside single quotes \" - 2'", SemiColon],
    [StaticSql "SELECT ", StaticSql "\"single quotes ' inside double quotes ' - 2\"", SemiColon],
    [ StaticSql $
        "$function$"
          <> "\nBEGIN"
          <> "\n    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$); /* Some would-be non terminated comment, but it's fine inside dollar quotes"
          <> "\nEND;"
          <> "\n$function$",
      SemiColon
    ],
    [StaticSql "SELECT COALESCE(4, 1 - 2) - 3 + 4 - 5;"],
    [StaticSql "SELECT (1 - 4) / 5 * 3 / 9.1;"],
    -- Semi-colons inside parenthesised blocks are not statement boundaries
    [StaticSql "create rule \n name as on \n some_event to \n\n some_table do (command1; command2; command3;);"],
    -- String blocks can be opened inside parentheses, just like comments, dollar-quoted strings and others
    [StaticSql "create rule name as on some_event to some_table do (", StaticSql "'abcdef);'", StaticSql "; other;)\n\n;"],
    [StaticSql "some statement with spaces (((U&", StaticSql "'d\\0061t\\+000061'", StaticSql ", ", StaticSql "'abc''def'", StaticSql " ; ; ; ", CommentsOrWhitespace "/* comment /* nested */ ((( */", StaticSql " ", StaticSql "$hey$dollar string$hey$", StaticSql ")) ", StaticSql "'more''of'", StaticSql "; this; ());"],
    [StaticSql "select ", StaticSql "'unclosed parentheses inside string ((((('", StaticSql ", ((", StaticSql "'string (('", StaticSql "));"],
    [ StaticSql $
        "CREATE STATISTICS IF NOT EXISTS test_stat_expr"
          <> "\n( dependencies,mcv) ON employee_id, lower(employee_name)"
          <> "\nFROM employee;"
    ],
    -- We still want the following to be parsed; it's best to run invalid statements than have
    -- a parser so strict that it might refuse valid ones.
    [StaticSql "invalid statement, bad parentheses ()));"],
    [StaticSql "begin;"],
    [StaticSql "BEGiN", CommentsOrWhitespace "/*a*/", SemiColon],
    [StaticSql "BEgIN \n  ;"],
    [StaticSql "ROllBaCk;"],
    [StaticSql "ROllBaCk", CommentsOrWhitespace "/*a*/", SemiColon],
    [StaticSql "ROllBaCk   ;"],
    [StaticSql "COmmIT;"],
    [StaticSql "COMMIT", CommentsOrWhitespace "/*a*/", SemiColon],
    [StaticSql "cOMMIT   ;"],
    [StaticSql "COPY employee FROM STDIN WITH (FORMAT CSV);"],
    [StaticSql "copy ", StaticSql "\"]schema\"", StaticSql ".employee FROM stdin \n WITH (FORMAT CSV);"],
    -- Fully qualified identifiers part 1 + table without columns, but with one row (this is possible!)
    [StaticSql "CoPy ", StaticSql "\"]some-database\"", StaticSql "   .  ", StaticSql "\"schema\"", StaticSql "  .  employee from stdin with \n (FORMAT CSV);"],
    -- Fully qualified identifiers part 2 + specifying columns
    [StaticSql "CoPy ", StaticSql "\"]employee\"", StaticSql " \n  (col1,", StaticSql "\"col2\"", StaticSql "   , \n\n  col4  ) from stdin with (FORMAT CSV);"]
  ]

newtype ShuffleOfPieces = ShuffleOfPieces [[BlockOrNotBlock]] deriving stock (Show)

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
              (5, blockListText <$> genSingleSqlStatement)
            ]
        else
          Gen.frequency
            [ (1, bizarreLineGen),
              (1, emptyLineGen),
              (5, blockListText <$> genSingleSqlStatement)
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
      atLeastOneStmt <- blockListText <$> genSingleSqlStatement
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
  describe "Parsing tests" $ parallel $ do
    context "Simple query parsing tests" $ do
      it "sql Quasiquoter without interpolation" $ do
        let query = getUniqueNE [sql|SELECT * FROM users|]
        query.queryString `shouldBe` encodeUtf8 "SELECT * FROM users"
        length query.queryParams `shouldBe` 0

      it "sql Quasiquoter with single interpolation" $ do
        let userId = 42 :: Int
            query = getUniqueNE [sql|SELECT * FROM users WHERE id = #{userId}|]
        query.queryString `shouldBe` encodeUtf8 "SELECT * FROM users WHERE id = $1"
        length query.queryParams `shouldBe` 1

      it "SQL string with question marks, not placeholders" $ do
        let qryStr = "/* Comment ??? */ SELECT 'text ? string ?' WHERE x=? AND y IN (?, ?, ?); SELECT ? FROM table"
            qry = mkQuery qryStr ()

        case breakQueryIntoStatements qry of
          (q1 :| [q2]) -> do
            q1.queryString `shouldBe` "/* Comment ??? */ SELECT 'text ? string ?' WHERE x=? AND y IN (?, ?, ?);"
            length q1.queryParams `shouldBe` 0
            q2.queryString `shouldBe` " SELECT ? FROM table"
            length q2.queryParams `shouldBe` 0
          _ -> expectationFailure "Expected the query to be composed of two statements"

      it "Supplying more params than placeholders throws an error" $ do
        let qryStr = "SELECT 1"
            qry = mkQuery qryStr (1 :: Int, 2 :: Int)
        result <- try @SomeException $ evaluate $ length $ NE.toList $ breakQueryIntoStatements qry
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected an error when supplying more params than placeholders"

      it "parses SQL with multiple interpolations" $ do
        let userId = 42 :: Int
            userName = "john" :: Text
            query = getUniqueNE [sql|SELECT * FROM users WHERE id = #{userId} AND name = #{userName}|]
        query.queryString `shouldBe` encodeUtf8 "SELECT * FROM users WHERE id = $1 AND name = $2"
        length query.queryParams `shouldBe` 2
    it "Single command with and without semi-colon" $ hedgehog $ do
      let stmts :: [[BlockOrNotBlock]] =
            map
              (parseSql AcceptOnlyDollarNumberedArgs)
              [ "CREATE TABLE hello;",
                "CREATE TABLE hello",
                "CREATE TABLE hello; -- Comment",
                "CREATE TABLE hello -- Comment",
                "CREATE TABLE hello -- Comment\n;"
              ]
      map flattenBlocks stmts
        === [ [StaticSql "CREATE TABLE hello", SemiColon],
              [StaticSql "CREATE TABLE hello"],
              [StaticSql "CREATE TABLE hello", SemiColon, CommentsOrWhitespace " -- Comment"],
              [StaticSql "CREATE TABLE hello ", CommentsOrWhitespace "-- Comment"],
              [StaticSql "CREATE TABLE hello ", CommentsOrWhitespace "-- Comment\n", SemiColon]
            ]
    it "Statement separation boundaries are good" $ hedgehog $ do
      origPieces <- fmap mconcat $ forAll $ Gen.list (Range.linear 1 100) genSingleSqlStatement
      let allSqlAsOneBigText :: Text = blockListText origPieces
      annotateShow allSqlAsOneBigText
      let parsedPieces = parseSql AcceptOnlyDollarNumberedArgs allSqlAsOneBigText
      blockListText parsedPieces
        === blockListText origPieces
    it "Statements concatenation matches original" $ hedgehog $ do
      SyntacticallyValidRandomSql {..} <- forAll genSyntacticallyValidRandomSql
      let stmts = parseSql AcceptOnlyDollarNumberedArgs unSyntRandomSql
      blockListText stmts === unSyntRandomSql

    it "parseSql AcceptQuasiQuoterExpressions preserves quasiquoter expressions with parentheses" $ do
      let input = "SELECT ^{escapeIdentifier (fromQuery name)}, #{someFunc (arg1) arg2}"
          result = parseSql AcceptQuasiQuoterExpressions input
          qqExprs = [(k, t) | QuasiQuoterExpression k t <- result]
      qqExprs `shouldBe` [(QQEmbeddedQuery, "escapeIdentifier (fromQuery name)"), (QQInterpolation, "someFunc (arg1) arg2")]

    it "parseSql AcceptQuasiQuoterExpressions handles nested parentheses in expressions" $ do
      let input = "SELECT #{f (g (x))}"
          result = parseSql AcceptQuasiQuoterExpressions input
          qqExprs = [(k, t) | QuasiQuoterExpression k t <- result]
      qqExprs `shouldBe` [(QQInterpolation, "f (g (x))")]

    it "parseSql AcceptQuasiQuoterExpressions inside parenthesised SQL expressions" $ do
      let input = "SELECT (#{someFunc (arg)})"
          result = parseSql AcceptQuasiQuoterExpressions input
          qqExprs = [(k, t) | QuasiQuoterExpression k t <- result]
      qqExprs `shouldBe` [(QQInterpolation, "someFunc (arg)")]

    it "parseSql AcceptQuasiQuoterExpressions handles } inside Haskell strings" $ do
      let input = "SELECT #{\"abc}\" ++ x}"
          result = parseSql AcceptQuasiQuoterExpressions input
          qqExprs = [(k, t) | QuasiQuoterExpression k t <- result]
      qqExprs `shouldBe` [(QQInterpolation, "\"abc}\" ++ x")]

    it "Sql Migration Parser never fails, even for random text" $ hedgehog $ do
      RandomSql {unRandomSql} <- forAll genRandomSql
      let stmts = parseSql AcceptOnlyDollarNumberedArgs unRandomSql
      let t = blockListText stmts
      t === unRandomSql

getUniqueNE :: (HasCallStack) => Query -> SingleQuery
getUniqueNE = NE.head . breakQueryIntoStatements
