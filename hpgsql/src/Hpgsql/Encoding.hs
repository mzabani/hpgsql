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
-- type check query results and field counts only once per query. If you need
-- to write a row decoder that is monadic (because decoding can change depending on the values
-- of fields), check "Hpgsql.Encoding.RowDecoderMonadic".
--
-- = Performant row decoders
--
-- Hpgsql provides roughly two* ways to derive row decoders from your types.
-- You can use `notInlinedSingleFieldRowDecoder` or `inlinedSingleFieldRowDecoder` for each field.
-- For example you can define:
--
-- > data Car = Car { model :: Text, year :: Maybe Int, inGoodCondition :: Bool }
-- >
-- > instance FromPgRow Car where
-- >   rowDecoder = Car <$> inlinedSingleFieldRowDecoder
-- >                    <*> inlinedSingleFieldRowDecoder
-- >                    <*> inlinedSingleFieldRowDecoder
--
-- And hpgsql will derive a row decoder that is extremely fast because almost all the
-- decoding code is inlined. Fully inlined row decoders can be ~15% faster than not fully
-- inlined row decoders.
--
-- However, bear in mind that fully inlined row decoders will generate more code and can possibly slow down compilation,
-- and that often the bottleneck is in query processing, not row decoding.
--
-- Some notes:
--
-- * Generically derived row decoders are not fully inlined, and perform as well as hand-written row decoders built with `notInlinedSingleFieldRowDecoder`.
-- * Another derivation method is to use `singleField fieldDecoder`. That is the least performant way of deriving row decoders, and is only useful if you need the ability to compose `FieldDecoder`s in ways that you can't otherwise. If you can, use `notInlinedSingleFieldRowDecoder` instead.
module Hpgsql.Encoding
  ( -- * Decoding
    FromPgField (fieldDecoder, notInlinedSingleFieldRowDecoder, inlinedSingleFieldRowDecoder), --  Do not export other methods so we can change them
    FieldDecoder (..),
    FieldInfo (..),
    FromPgRow (..),
    RowDecoder (..), -- TODO: Can we export ctor?
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
