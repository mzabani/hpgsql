{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
    FieldParser,
    Conversion (),
    runConversion,
    conversionMap,
    conversionError,
    ResultError (..),
    returnError,
    Field,
    typename,
    TypeInfo (..),
    Attribute (..),
    typeInfo,
    typeInfoByOid,
    name,
    typeOid,
    PQ.Oid (..),
    PQ.Format (..),
    pgArrayFieldParser,
    attoFieldParser,
    optionalField,
    fromJSONField,
    fromFieldJSONByteString,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (Exception (fromException, toException))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import Data.Typeable (Typeable, typeOf)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Arrays as Arrays
import Database.PostgreSQL.Simple.Compat
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import Database.PostgreSQL.Simple.Types
import HPgsql.Encoding (FromPgField (..))
import qualified HPgsql.Encoding as HPgsql
import HPgsql.TypeInfo (Format (..))
import qualified HPgsql.TypeInfo as HPgsqlTI
import System.IO.Unsafe (unsafePerformIO)

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

left :: (Exception a) => a -> Conversion b
left = conversionError

type FieldParser a = Field -> Maybe ByteString -> Conversion a

-- | A type that may be converted from a SQL type.
class FromField a where
  fromField :: FieldParser a
  -- ^ Convert a SQL value to a Haskell value.
  --
  -- Returns a list of exceptions if the conversion fails.  In the case of
  -- library instances,  this will usually be a single 'ResultError',  but
  -- may be a 'UnicodeException'.
  --
  -- Note that retaining any reference to the 'Field' argument causes
  -- the entire @LibPQ.'PQ.Result'@ to be retained.  Thus, implementations
  -- of 'fromField' should return results that do not refer to this value
  -- after the result have been evaluated to WHNF.
  --
  -- Note that as of @hpgsql-simple-compat-0.4.0.0@,  the 'ByteString' value
  -- has already been copied out of the @LibPQ.'PQ.Result'@ before it has
  -- been passed to 'fromField'.  This is because for short strings, it's
  -- cheaper to copy the string than to set up a finalizer.

-- | The instances derived here rely on unsafePerformIO and bottoms in lots
-- of places. They should be fine for most commonly found FromField instances
-- out there, but will crash and burn for others.
-- This is also an orphan instance, but we choose to accept that in the name
-- of pragmatism: users wanting to migrate to hpgsql will likely find this easier
-- than alternatives.
instance {-# OVERLAPPABLE #-} (FromField a) => FromPgField a where
  fieldParser =
    HPgsql.FieldParser
      { HPgsql.fieldValueParser = \colInfo mbs ->
          let field = Field {result = error "Field.result not available in hpgsql-simple-compat", column = error "Field.column not available in hpgsql-simple-compat", typeOid = fromHpgsqlOid $ HPgsql.typeOid colInfo}
              strictBs = fmap LB.toStrict mbs
           in unsafePerformIO $ do
                -- Build a Connection with a pre-populated TypeInfoCache from
                -- HPgsql's typeInfoCache. This allows 'typename', 'returnError',
                -- and 'typeInfo' to work for any type in the cache.
                connectionObjects <- newMVar $ hpgsqlTypeInfoCacheToCompat (HPgsql.typeInfoCache colInfo)
                let conn =
                      Connection
                        connectionObjects
                        (error "Connection not available in FromField instances in hpgsql-simple-compat")
                        (error "Connection not available in FromField instances in hpgsql-simple-compat")
                ok <- runConversion (fromField field strictBs) conn
                case ok of
                  Ok a -> pure (Right a)
                  Errors errs -> pure (Left (show errs)),
        HPgsql.fieldFmt = BadlySupportedTextFmt,
        HPgsql.allowedPgTypes = const True
      }

-- | Converts HPgsql's TypeInfo cache to one similar to postgresql-simple's, but
-- with lots of bottoms everywhere because hpgsql's typeInfo cache is less complete.
hpgsqlTypeInfoCacheToCompat :: Map.Map HPgsqlTI.Oid HPgsqlTI.TypeInfo -> TypeInfoCache
hpgsqlTypeInfoCacheToCompat = IntMap.fromList . map convert . Map.toList
  where
    convert (hoid, ti) =
      let pqOid = fromHpgsqlOid hoid
       in ( oid2int pqOid,
            Basic
              { typoid = pqOid,
                typcategory = error "typcategory not available in hpgsql-simple-compat",
                typdelim = ',',
                typname = ST.encodeUtf8 (HPgsqlTI.typeName ti)
              }
          )

-- | Returns the data type name.  This is the preferred way of identifying
--   types that do not have a stable type oid, such as types provided by
--   extensions to PostgreSQL.
--
--   More concretely,  it returns the @typname@ column associated with the
--   type oid in the @pg_type@ table.  First, hpgsql-simple-compat will check
--   the built-in, static table.   If the type oid is not there,
--   hpgsql-simple-compat will check a per-connection cache,  and then
--   finally query the database's meta-schema.
typename :: Field -> Conversion ByteString
typename field = typname <$> typeInfo field

typeInfo :: Field -> Conversion TypeInfo
typeInfo Field {..} = Conversion $ \conn -> do
  Ok <$> (getTypeInfo conn typeOid)

typeInfoByOid :: PQ.Oid -> Conversion TypeInfo
typeInfoByOid oid = Conversion $ \conn -> do
  Ok <$> (getTypeInfo conn oid)

-- | Returns the name of the column.  This is often determined by a table
--   definition,  but it can be set using an @as@ clause.
name :: Field -> Maybe ByteString
name Field {..} = unsafeDupablePerformIO (PQ.fname result column)

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


pgArrayFieldParser :: (Typeable a) => FieldParser a -> FieldParser (PGArray a)
pgArrayFieldParser fieldParser f mdat = do
  info <- typeInfo f
  case info of
    TI.Array {} ->
      case mdat of
        Nothing -> returnError UnexpectedNull f ""
        Just dat -> do
          case parseOnly (fromArray fieldParser info f) dat of
            Left err -> returnError ConversionFailed f err
            Right conv -> PGArray <$> conv
    _ -> returnError Incompatible f ""

fromArray :: FieldParser a -> TypeInfo -> Field -> Parser (Conversion [a])
fromArray fieldParser typInfo f = sequence . (parseIt <$>) <$> array delim
  where
    delim = typdelim (typelem typInfo)
    fElem = f {typeOid = typoid (typelem typInfo)}

    parseIt item =
      fieldParser f' $ if item == Arrays.Plain "NULL" then Nothing else Just item'
      where
        item' = fmt delim item
        f'
          | Arrays.Array _ <- item = f
          | otherwise = fElem

-- | Return the JSON ByteString directly
--
-- @since 0.6.3
fromFieldJSONByteString :: Field -> Maybe ByteString -> Conversion ByteString
fromFieldJSONByteString f mbs =
  if typeOid f /= TI.jsonOid && typeOid f /= TI.jsonbOid
    then returnError Incompatible f ""
    else case mbs of
      Nothing -> returnError UnexpectedNull f ""
      Just bs -> pure bs

-- | Parse a field to a JSON 'JSON.Value' and convert that into a
-- Haskell value using the 'JSON.FromJSON' instance.
--
-- This can be used as the default implementation for the 'fromField'
-- method for Haskell types that have a JSON representation in
-- PostgreSQL.
--
-- The 'Typeable' constraint is required to show more informative
-- error messages when parsing fails.
--
-- Note that @fromJSONField :: FieldParser ('Maybe' Foo)@ will return
-- @'Nothing'@ on the json @null@ value, and return an exception on SQL @null@
-- value.  Alternatively,  one could write @'optionalField' fromJSONField@
-- that will return @Nothing@ on SQL @null@,  and otherwise will call
-- @fromJSONField :: FieldParser Foo@ and then return @'Just'@ the
-- result value,  or return its exception.  If one would
-- like to return @Nothing@ on both the SQL @null@ and json @null@ values,
-- one way to do it would be to write
-- @\\f mv -> 'Control.Monad.join' '<$>' optionalField fromJSONField f mv@
fromJSONField :: (JSON.FromJSON a, Typeable a) => FieldParser a
fromJSONField f mbBs = do
  bs <- fromFieldJSONByteString f mbBs
  value <- case JSON.eitherDecodeStrict' bs of
    Left err -> returnError ConversionFailed f $ "JSON parse error: " ++ err
    Right val -> pure val
  case JSON.ifromJSON value of
    JSON.IError path err ->
      returnError ConversionFailed f $
        "JSON decoding error: " ++ (JSON.formatError path err)
    JSON.ISuccess x -> pure x

type Compat = PQ.Oid -> Bool

doFromField ::
  forall a.
  (Typeable a) =>
  Field ->
  Compat ->
  (ByteString -> Conversion a) ->
  Maybe ByteString ->
  Conversion a
doFromField f isCompat cvt (Just bs)
  | isCompat (typeOid f) = cvt bs
  | otherwise = returnError Incompatible f "types incompatible"
doFromField f _ _ _ = returnError UnexpectedNull f ""

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
      (maybe "" B8.unpack (name f))
      (show (typeOf (undefined :: a)))
      msg

-- | Construct a field parser from an attoparsec parser. An 'Incompatible' error is thrown if the
-- PostgreSQL oid does not match the specified predicate.
--
-- @
-- instance FromField Int16 where
--   fromField = attoFieldParser ok16 (signed decimal)
-- @
--
-- @since 0.6.3
attoFieldParser ::
  forall a.
  (Typeable a) =>
  -- | Predicate for whether the postgresql type oid is compatible with this parser
  (PQ.Oid -> Bool) ->
  -- | An attoparsec parser.
  Parser a ->
  FieldParser a
attoFieldParser types p0 f dat = doFromField f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Conversion a
    go p s =
      case parseOnly p s of
        Left err -> returnError ConversionFailed f err
        Right v -> pure v
