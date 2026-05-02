{-# LANGUAGE FlexibleInstances #-}

module Database.PostgreSQL.Query.SqlBuilder.Class
  ( ToSqlBuilder (..),
  )
where

import Database.PostgreSQL.Query.SqlBuilder.Builder (SqlBuilder)

-- | Typeclass for types convertible to 'SqlBuilder'.
class ToSqlBuilder a where
  toSqlBuilder :: a -> SqlBuilder

instance ToSqlBuilder SqlBuilder where
  toSqlBuilder = id
