-- |
--
-- = Encoding and decoding fields and rows
--
-- This module contains a collection of functions, classes, and instances that can
-- help you build encoders and decoders for your Haskell types.
--
-- Here's an example:
--
-- > data Person = Person { name :: Text, born :: Day, heightMeters :: Double }
-- >   deriving stock Generic
-- >   deriving anyclass FromPgRow
-- >
-- > persons :: [Person] <- query conn "SELECT * FROM persons"
--
-- Note that Hpgsql's `RowDecoder` does not have a `Monad` instance because that allows it to
-- type check query results and field counts only once per query instead of paying that price
-- for every field of every row. If you need to write a row decoder that is monadic, check "Hpgsql.Encoding.RowDecoderMonadic".
--
-- = Manually derived row decoders
--
-- You can use `fieldRowDecoder` for each field in manually derived row decoders.
-- For example:
--
-- > data Car = Car { model :: Text, year :: Maybe Int, inGoodCondition :: Bool }
-- >
-- > instance FromPgRow Car where
-- >   rowDecoder = Car <$> fieldRowDecoder
-- >                    <*> fieldRowDecoder
-- >                    <*> fieldRowDecoder
--
-- In our benchmarks hand written FromPgRow instances aren't any more performant than Generically derived ones.
--
-- Noteworthy is you should not use `singleField fieldDecoder` when you can use `fieldRowDecoder` instead. The former
-- is much slower, and Hpgsql even has rewrite rules to rewrite it when compiling with -O.
module Hpgsql.Encoding
  ( -- * Decoding
    FromPgField (fieldDecoder, fieldRowDecoder), --  Do not export other methods so we can change them
    FieldDecoder (..),
    FieldInfo (..),
    FromPgRow (..),
    RowDecoder (..), -- TODO: We should consider not exporting everything to give us freedom
    singleField,
    nullableField,
    genericFromPgRow,

    -- * Encoding

    -- | Keep in mind that Haskell's `Int` is an `Int64` on most
    -- hardware, which is a mismatch for the commonly used 32-bit
    -- `integer` PostgreSQL type.
    -- This is not a problem when decoding because Hpgsql can decode
    -- `integer` into Haskell's `Int`, but when encoding PostgreSQL
    -- will understandably not accept a larger type.
    ToPgField (..),
    FieldEncoder (..),
    ToPgRow (..),
    RowEncoder (..),
    EncodingContext (..),
    genericToPgRow,

    -- * PostgreSQL enums
    LowerCasedPgEnum (..),
    genericEnumFieldDecoder,
    genericEnumFieldEncoder,

    -- * PostgreSQL composite types
    compositeTypeDecoder,
    compositeTypeEncoder,

    -- * Driving PostgreSQL type inference
    typeFieldDecoder,
    typeFieldEncoder,
    typeOidWithName,
    typeMustBeNamed,

    -- * Others
    rawBytesFieldDecoder,
    untypedFieldEncoder,
    toPgVectorField,
    arrayField,
  )
where

import Hpgsql.Encoding.Internal
