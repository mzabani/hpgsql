{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module:      Database.PostgreSQL.Simple.FromField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- The 'FromField' typeclass, for converting a single value in a row
-- returned by a SQL query into a more useful Haskell representation.
-- Note that each instance of 'FromField' is documented by a list of
-- compatible postgresql types.
--
-- A Haskell numeric type is considered to be compatible with all
-- PostgreSQL numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the PostgreSQL's 32-bit
-- @int@ type because it can represent a @int@ exactly.  On the other hand,
-- since a 'Double' might lose precision if representing PostgreSQL's 64-bit
-- @bigint@, the two are /not/ considered compatible.
--
-- Note that the 'Float' and 'Double' instances use attoparsec's 'double'
-- conversion routine,  which sacrifices some accuracy for speed.   If you
-- need accuracy,  consider first converting data to a 'Scientific' or 'Rational'
-- type,  and then converting to a floating-point type.   If you are defining
-- your own 'Database.PostgreSQL.Simple.FromRow.FromRow' instances,  this can be
-- achieved simply by
-- @'fromRational' '<$>' 'Database.PostgreSQL.Simple.FromRow.field'@,  although
-- this idiom is additionally compatible with PostgreSQL's @int8@ and @numeric@
-- types.  If this is unacceptable,  you may find
-- 'Database.PostgreSQL.Simple.FromRow.fieldWith' useful.
--
-- Also note that while converting to a 'Double' through the 'Scientific' type
-- is likely somewhat faster than converting through the 'Rational' type,
-- the 'Scientific' type has no way to represent @NaN@ and @±Infinity@ values.
-- Thus,  if you need precision conversion of regular floating point values
-- and the possibility of receiving these special values from the backend,
-- stick with 'Rational'.
--
-- Because 'FromField' is a typeclass,  one may provide conversions to
-- additional Haskell types without modifying hpgsql-simple-compat.  This is
-- particularly useful for supporting PostgreSQL types that hpgsql-simple-compat
-- does not support out-of-box.  Here's an example of what such an instance
-- might look like for a UUID type that implements the @Read@ class:
--
-- @
-- import Data.UUID ( UUID )
-- import Database.PostgreSQL.Simple.FromField
--        ( FromField (fromField) , typeOid, returnError, ResultError (..) )
-- import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, uuid)
-- import qualified Data.ByteString.Char8 as B
--
-- instance FromField UUID where
--    fromField f mdata =
--       if typeOid f /= typoid uuid
--         then returnError Incompatible f \"\"
--         else case B.unpack \`fmap\` mdata of
--                Nothing  -> returnError UnexpectedNull f \"\"
--                Just dat ->
--                   case [ x | (x,t) <- reads dat, (\"\",\"\") <- lex t ] of
--                     [x] -> return x
--                     _   -> returnError ConversionFailed f dat
-- @
--
-- Note that because PostgreSQL's @uuid@ type is built into postgres and is
-- not provided by an extension,  the 'typeOid' of @uuid@ does not change and
-- thus we can examine it directly.  One could hard-code the type oid,  or
-- obtain it by other means, but in this case we simply pull it out of the
-- static table provided by hpgsql-simple-compat.
--
-- On the other hand if the type is provided by an extension,  such as
-- @PostGIS@ or @hstore@,  then the 'typeOid' is not stable and can vary from
-- database to database. In this case it is recommended that FromField
-- instances use 'typename' instead.
module Database.PostgreSQL.Simple.FromField
  ( FromField (..),
    FromPgField (..),
    FieldParser,
    Conversion (..),
    -- conversionMap,
    conversionError,
    ResultError (..),
    returnError,
    Field,
    typename,
    TypeInfo (..),
    Attribute (..),
    typeInfo,
    typeInfoByOid,
    -- name,
    typeOid,
    PQ.Oid (..),
    PQ.Format (..),
    optionalField,
    fromFieldJSONByteString,
    fromJSONField,
  )
where

import Control.Exception (Exception (fromException, toException))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar.Compat (Day)
import Data.Time.Compat (UTCTime)
import Data.Time.LocalTime.Compat (CalendarDiffTime, LocalTime, ZonedTime)
import Data.Typeable (Typeable, typeOf)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.HpgsqlUtils
import Database.PostgreSQL.Simple.Internal (postgresqlExceptionFromException, postgresqlExceptionToException)
import Database.PostgreSQL.Simple.Ok (Ok (..))
import Database.PostgreSQL.Simple.TypeInfo as TI
import Database.PostgreSQL.Simple.TypeInfo.Types (fromHpgsqlTypeInfo)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Hpgsql.Encoding (FieldInfo (..), EncodingContext (..), FromPgField (..))
import qualified Hpgsql.Encoding as Hpgsql
import Hpgsql.Time (Unbounded (..))
import Hpgsql.TypeInfo (Oid, lookupTypeByOid)
import Hpgsql.Types (Aeson, PGArray (..), getAeson, pgJsonByteString)

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError
  = -- | The SQL and Haskell types are not compatible.
    Incompatible
      { errSQLType :: String,
        errSQLTableOid :: Maybe PQ.Oid,
        errSQLField :: String,
        errHaskellType :: String,
        errMessage :: String
      }
  | -- | A SQL @NULL@ was encountered when the Haskell
    -- type did not permit it.
    UnexpectedNull
      { errSQLType :: String,
        errSQLTableOid :: Maybe PQ.Oid,
        errSQLField :: String,
        errHaskellType :: String,
        errMessage :: String
      }
  | -- | The SQL value could not be parsed, or could not
    -- be represented as a valid Haskell value, or an
    -- unexpected low-level error occurred (e.g. mismatch
    -- between metadata and actual data in a row).
    ConversionFailed
      { errSQLType :: String,
        errSQLTableOid :: Maybe PQ.Oid,
        errSQLField :: String,
        errHaskellType :: String,
        errMessage :: String
      }
  deriving (Eq, Show, Typeable)

instance Exception ResultError where
  toException = postgresqlExceptionToException
  fromException = postgresqlExceptionFromException

-- | This might be very different from postgresql-simple.
conversionError :: (Exception err) => err -> Conversion a
conversionError ex = Conversion $ \_ -> Errors [toException ex]

left :: (Exception a) => a -> Conversion b
left = conversionError

class FromField a where
  fromField :: FieldParser a
  default fromField :: (Hpgsql.FromPgField a) => FieldParser a
  fromField =
    let dec = Hpgsql.fieldDecoder
     in \f ->
          if (Hpgsql.allowedPgTypes dec) f
            then \mbs -> Conversion $ \_encCtx -> case (Hpgsql.fieldValueDecoder dec) f mbs of
              Right v -> Ok v
              Left err -> Errors [toException $ userError err]
            else \_ -> Conversion $ \_encCtx -> Errors [toException $ userError "Invalid type OID for FromField instance"]

instance FromField ()

instance FromField Int

instance FromField Int16

instance FromField Int32

instance FromField Int64

instance FromField Integer

instance FromField Oid

instance FromField Scientific

instance FromField Rational

instance FromField Float

instance FromField Double

instance FromField Bool

instance FromField Char

instance FromField ByteString

instance FromField LBS.ByteString

instance FromField Text

instance FromField LT.Text

instance FromField String

instance FromField UTCTime

instance FromField ZonedTime

instance FromField LocalTime

instance FromField Day

instance FromField CalendarDiffTime

instance FromField Aeson.Value

instance FromField UUID

instance FromField (CI Text)

instance FromField (CI LT.Text)

instance FromField (CI String)

instance FromField (Unbounded Day)

instance FromField (Unbounded UTCTime)

instance FromField (Unbounded ZonedTime)

instance (FromField a) => FromField (Maybe a) where
  fromField = optionalField fromField

-- instance (FromField a) => FromField (Vector a)

-- instance (FromField a) => FromField (PGArray a)

instance (Hpgsql.FromPgField a) => FromField (PGArray a)

instance (Hpgsql.FromPgField a) => FromField (Vector a)

instance {-# OVERLAPPING #-} (Hpgsql.FromPgField a) => FromField (Vector (Vector a))

instance FromField (Binary ByteString)

instance (Aeson.FromJSON a) => FromField (Aeson a)

typeOid :: Field -> Oid
typeOid field = field.fieldTypeOid

typename :: Field -> Conversion ByteString
typename field = typname <$> typeInfo field

typeInfo :: Field -> Conversion TypeInfo
typeInfo colInfo = typeInfoByOid colInfo.fieldTypeOid

typeInfoByOid :: PQ.Oid -> Conversion TypeInfo
typeInfoByOid oid = Conversion $ \encCtx -> do
  case lookupTypeByOid oid encCtx.typeInfoCache of
    Nothing -> Errors [toException $ userError $ "Type OID " ++ show oid ++ " not found in TypeInfoCache"]
    Just t -> Ok (fromHpgsqlTypeInfo t)

-- | Returns the name of the column.  This is often determined by a table
--   definition,  but it can be set using an @as@ clause.
-- name :: Field -> Maybe ByteString
-- name Field {..} = unsafeDupablePerformIO (PQ.fname result column)

-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError ::
  forall a err.
  (Typeable a, Exception err) =>
  (String -> Maybe PQ.Oid -> String -> String -> String -> err) ->
  Field ->
  String ->
  Conversion a
returnError mkErr f msg = do
  typnam <- typename f
  left $
    mkErr
      (B8.unpack typnam)
      (error "no tableOid in hpgsql")
      -- (maybe "" B8.unpack (name f))
      "column name not available in hpgsql-simple-compat"
      (show (typeOf (undefined :: a)))
      msg

-- | For dealing with SQL @null@ values outside of the 'FromField' class.
--   Alternatively, one could use 'Control.Applicative.optional',  but that
--   also turns type and conversion errors into 'Nothing',  whereas this is
--   more specific and turns only @null@ values into 'Nothing'.
optionalField :: FieldParser a -> FieldParser (Maybe a)
optionalField p f mv =
  case mv of
    Nothing -> pure Nothing
    Just _ -> Just <$> p f mv
{-# INLINE optionalField #-}

fromJSONField :: (Aeson.FromJSON a) => FieldParser a
fromJSONField f mbs = getAeson <$> fromField f mbs

fromFieldJSONByteString :: FieldParser ByteString
fromFieldJSONByteString = fromHpgsqlFieldDecoder $ pgJsonByteString <$> fieldDecoder
