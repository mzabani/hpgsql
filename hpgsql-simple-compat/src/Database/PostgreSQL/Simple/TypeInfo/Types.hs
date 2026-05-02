
------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.TypeInfo.Types
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
module Database.PostgreSQL.Simple.TypeInfo.Types where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Database.PostgreSQL.LibPQ (Oid)
import qualified Hpgsql.TypeInfo as Hpgsql

data TypeInfo
  = -- We still need to support more fields here!

    Basic
      { typoid :: Oid,
        typcategory :: Char,
        typdelim :: Char,
        typname :: ByteString
      }
  | Array
      { typoid :: Oid,
        typcategory :: Char,
        typdelim :: Char,
        typname :: ByteString,
        typelem :: TypeInfo
      }
  | Composite
      { typoid :: {-# UNPACK #-} !Oid,
        typcategory :: {-# UNPACK #-} !Char,
        typdelim :: {-# UNPACK #-} !Char,
        typname :: !ByteString,
        typrelid :: {-# UNPACK #-} !Oid,
        attributes :: !(Vector Attribute)
      }
  | Range
      { typoid :: {-# UNPACK #-} !Oid,
        typcategory :: {-# UNPACK #-} !Char,
        typdelim :: {-# UNPACK #-} !Char,
        typname :: !ByteString,
        rngsubtype :: !TypeInfo
      }
  deriving
    ( Show
    )

fromHpgsqlTypeInfo :: Hpgsql.TypeInfo -> TypeInfo
fromHpgsqlTypeInfo ti = case ti.oidOfArrayType of
  Nothing ->
    Array
      { typoid = ti.typeOid,
        typcategory = error "typcategory not available in hpgsql-simple-compat",
        typdelim = ',',
        typname = encodeUtf8 ti.typeName,
        typelem = error "typelem not available in hpgsql-simple-compat"
      }
  Just _arrayTyOid ->
    Basic
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName,
        typcategory = error "typcategory not available in hpgsql-simple-compat",
        typdelim = error "typdelim not available in hpgsql-simple-compat"
      }

data Attribute
  = Attribute
  { attname :: !ByteString,
    atttype :: !TypeInfo
  }
  deriving (Show)
