
------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.TypeInfo
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- This module provides convenient and efficient access to parts of the
-- @pg_type@ metatable.  At the moment, this requires PostgreSQL 8.4 if
-- you need to work with types that do not appear in
-- 'Database.PostgreSQL.Simple.TypeInfo.Static'.
--
-- The current scheme could be more efficient, especially for some use
-- cases.  In particular,  connection pools that use many user-added
-- types and connect to a set of servers with identical (or at least
-- compatible) @pg_type@ and associated tables could share a common
-- typeinfo cache,  thus saving memory and communication between the
-- client and server.
module Database.PostgreSQL.Simple.TypeInfo
  ( getTypeInfo,
    TypeInfo (..),
    Attribute (..),
  )
where

import Control.Concurrent.MVar
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.TypeInfo.Static
import Database.PostgreSQL.Simple.TypeInfo.Types
import Hpgsql.Encoding ()

-- | Returns the metadata of the type with a particular oid.  To find
--   this data, 'getTypeInfo' first consults hpgsql-simple-compat's
--   built-in 'staticTypeInfo' table,  then checks  the connection's
--   typeinfo cache.   Finally,  the database's 'pg_type' table will
--   be queried only if necessary,  and the result will be stored
--   in the connections's cache.
getTypeInfo :: Connection -> PQ.Oid -> IO TypeInfo
getTypeInfo conn@Connection {..} oid' =
  case staticTypeInfo oid' of
    Just name' -> return name'
    Nothing -> modifyMVar connectionObjects $ getTypeInfo' conn oid'

getTypeInfo' ::
  Connection ->
  PQ.Oid ->
  TypeInfoCache ->
  IO (TypeInfoCache, TypeInfo)
getTypeInfo' _conn _oid' _oidmap = error "TODO getTypeInfo'"
