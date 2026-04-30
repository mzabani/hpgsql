module Hpgsql.Encoding.RowDecoderMonadic
  ( RowDecoderMonadic (..), -- TODO: Can we export ctor?
    ConversionState (..),
    toMonadicRowDecoder,
  )
where

import Data.Bifunctor (first)
import qualified Data.List as List
import Hpgsql.Encoding (FieldInfo, RowDecoder (..))
import qualified Hpgsql.SimpleParser as Parser

-- | Unlike @Hpgsql.Encoding.RowDecoder@, this has a @Monad@ instance.
-- You should prefer to use @Hpgsql.Encoding.RowDecoder@ (through @FromPgRow@ instances)
-- instead of this, and use this only if your row decoder is complex enough that
-- decoded fields can change the behaviour of other decoded fields.
-- The regular @RowDecoder@ can even type-check queries that return no results, while
-- this can't.
-- Look for the 'query' and 'pipeline' functions with an 'M' in them for ways to query
-- with this kind of row decoder.
newtype RowDecoderMonadic a = RowDecoderMonadic
  { -- | Returns the parsed row and the number of columns parsed
    fullRowDecoder :: ConversionState -> Parser.Parser (a, Int)
  }

newtype ConversionState = ConversionState
  { colsLeftToParse :: [FieldInfo]
  }

instance Functor RowDecoderMonadic where
  fmap f (RowDecoderMonadic {fullRowDecoder}) = RowDecoderMonadic $ \cs -> fmap (first f) (fullRowDecoder cs)

instance Applicative RowDecoderMonadic where
  pure v = RowDecoderMonadic $ \_cs -> pure (v, 0)
  RowDecoderMonadic {fullRowDecoder = rpF} <*> RowDecoderMonadic {fullRowDecoder = rpV} = RowDecoderMonadic $ \cs -> do
    (rowF, n1) <- rpF cs
    (rowV, n2) <- rpV cs {colsLeftToParse = List.drop n1 cs.colsLeftToParse}
    pure (rowF rowV, n1 + n2)

instance Monad RowDecoderMonadic where
  RowDecoderMonadic {fullRowDecoder} >>= f = RowDecoderMonadic $ \cs0 -> do
    (row, numColsParsed) <- fullRowDecoder cs0
    let RowDecoderMonadic {fullRowDecoder = parserOfRemainder} = f row
    parserOfRemainder cs0 {colsLeftToParse = List.drop numColsParsed cs0.colsLeftToParse}

-- | Takes an Applicative row parser (which can type-check result rows before even fetching
-- any rows from the response) and transforms it into a Monadic row parser, which has no such
-- type-checking.
toMonadicRowDecoder :: RowDecoder a -> RowDecoderMonadic a
toMonadicRowDecoder RowDecoder {fullRowDecoder, numExpectedColumns} = RowDecoderMonadic $ \cs -> do
  let numActualCols = length cs.colsLeftToParse
  case compare numActualCols numExpectedColumns of
    EQ -> (,numExpectedColumns) <$> fullRowDecoder cs.colsLeftToParse
    GT -> (,numExpectedColumns) <$> fullRowDecoder (List.take numExpectedColumns cs.colsLeftToParse)
    LT -> fail $ "More number of columns expected by the row parser than found in query results. Expected " ++ show numExpectedColumns ++ " but got " ++ show numActualCols
