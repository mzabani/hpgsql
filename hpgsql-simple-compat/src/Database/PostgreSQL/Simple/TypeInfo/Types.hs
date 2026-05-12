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
import Database.PostgreSQL.LibPQ (Oid)
import Hpgsql.TypeInfo (ArrayTypeDetails (..), TypeDetails (..), lookupTypeByOid)
import qualified Hpgsql.TypeInfo as Hpgsql

-- | This is not exactly like postgresql-simple's TypeInfo,
-- since some fields are slightly altered and others have been removed.
data TypeInfo
  = Basic
      { typoid :: Oid,
        -- typcategory :: Char,
        -- typdelim :: Char,
        typname :: ByteString
      }
  | Array
      { typoid :: Oid,
        -- typcategory :: Char,
        -- typdelim :: Char,
        typname :: ByteString,
        -- | In postgresql-simple typelem is not a Maybe
        typelem :: Maybe TypeInfo
      }
  | Composite
      { typoid :: Oid,
        -- typcategory :: Char,
        -- typdelim :: Char,
        typname :: ByteString
        -- typrelid :: Oid,
        -- attributes :: (Vector Attribute)
      }
  | Range
      { typoid :: Oid,
        -- typcategory :: Char,
        -- typdelim :: Char,
        typname :: ByteString
        -- rngsubtype :: TypeInfo
      }
  deriving
    ( Show
    )

fromHpgsqlTypeInfo :: Hpgsql.EncodingContext -> Hpgsql.TypeInfo -> TypeInfo
fromHpgsqlTypeInfo encCtx ti = case ti.typeDetails of
  ArrayType arrDetails ->
    Array
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName,
        typelem = fromHpgsqlTypeInfo encCtx <$> lookupTypeByOid arrDetails.elemTypeOid encCtx.typeInfoCache
      }
  BasicType ->
    Basic
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
      }
  CompositeType ->
    Composite
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
      }
  RangeType ->
    Range
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
        -- rngsubtype = error "rngsubtype not available in hpgsql-simple-compat"
      }
  DomainType ->
    Basic
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
      }
  EnumType ->
    Basic
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
      }
  PseudoType ->
    Basic
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
      }
  MultiRangeType ->
    Basic
      { typoid = ti.typeOid,
        typname = encodeUtf8 ti.typeName
      }

data Attribute
  = Attribute
  { attname :: !ByteString,
    atttype :: !TypeInfo
  }
  deriving (Show)
