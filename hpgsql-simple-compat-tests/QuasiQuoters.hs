{-# LANGUAGE QuasiQuotes #-}

module QuasiQuoters (testQuasiQuoters) where

import Common
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Data.Text (Text)
import Database.PostgreSQL.Query.Functions (pgQuery)
import Database.PostgreSQL.Query.TH.SqlExp (sqlExp)
import Database.PostgreSQL.Query.Types (FN (..), fnToQuery, runPgMonadT)
import Database.PostgreSQL.Simple.HpgsqlUtils (toPgSimpleQuery)
import Hpgsql.Query (mkQuery)
import Test.Tasty

testQuasiQuoters :: TestEnv -> TestTree
testQuasiQuoters env =
  testGroup
    "QuasiQuoters"
    [ testCase "sqlExp no arguments" $ testSqlExpNoArgs env,
      testCase "sqlExp with arguments" $ testSqlExpWithArgs env,
      testCase "sqlExp with multiple arguments" $ testSqlExpMultipleArgs env,
      testCase "toPgSimpleQuery with repeated args" $ testRepeatedArgs env,
      testCase "sqlExp dynamic query with FN" $ testSqlExpDynamicQuery env
    ]

testSqlExpNoArgs :: TestEnv -> Assertion
testSqlExpNoArgs TestEnv {..} = do
  let (q, row) = toPgSimpleQuery [sqlExp|SELECT 1::int, 'hello'::text|]
  res <- query conn q row
  res @?= [(1 :: Int, "hello" :: Text)]

testSqlExpWithArgs :: TestEnv -> Assertion
testSqlExpWithArgs TestEnv {..} = do
  let x = 42 :: Int
      (q, row) = toPgSimpleQuery [sqlExp|SELECT #{x}::int|]
  res <- query conn q row
  res @?= [Only (42 :: Int)]

testSqlExpMultipleArgs :: TestEnv -> Assertion
testSqlExpMultipleArgs TestEnv {..} = do
  let x = 42 :: Int
      y = "world" :: Text
      (q, row) = toPgSimpleQuery [sqlExp|SELECT #{x}::int, #{y}::text|]
  res <- query conn q row
  res @?= [(42 :: Int, "world" :: Text)]

testRepeatedArgs :: TestEnv -> Assertion
testRepeatedArgs TestEnv {..} = do
  let hpgsqlQuery = mkQuery "SELECT $1::int, $1::int, $2, $3, $2" (42 :: Int, "a" :: String, True)
      (q, row) = toPgSimpleQuery hpgsqlQuery
  res <- query conn q row
  res @?= [(42 :: Int, 42 :: Int, "a" :: String, True, "a" :: String)]

testSqlExpDynamicQuery :: TestEnv -> Assertion
testSqlExpDynamicQuery TestEnv {..} = do
  execute_ conn "CREATE TEMPORARY TABLE users (id INT, name TEXT, age INT)"
  execute_ conn "INSERT INTO users VALUES (1, 'Simon Peyton Jones', 30)"
  execute_ conn "INSERT INTO users VALUES (2, 'John Smith', 5)"
  execute_ conn "INSERT INTO users VALUES (3, 'Peyton Manning', 45)"

  -- Dynamic query building, following the postgresql-query README
  let name = Just "%Peyton%" :: Maybe Text
      minage = Just (10 :: Int)
      maxage = Just (50 :: Int)
      ord = "u.name" :: FN
      condlist = catMaybes
        [ fmap (\a -> [sqlExp|u.name LIKE #{a}|]) name
        , fmap (\a -> [sqlExp|u.age > #{a}|]) minage
        , fmap (\a -> [sqlExp|u.age < #{a}|]) maxage
        ]
      conds = case condlist of
        [] -> ""
        _  -> " WHERE " <> foldr1 (<>) (intersperse " AND " condlist)
  res <- runPgMonadT conn $
    pgQuery [sqlExp|SELECT u.id, u.name, u.age FROM users AS u ^{conds} ORDER BY ^{fnToQuery ord}|]

  res @?= [ (3 :: Int, "Peyton Manning" :: Text, 45 :: Int)
          , (1, "Simon Peyton Jones", 30)
          ]
