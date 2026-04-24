module Hpgsql.Encoding.RowParserMonadic
  ( RowParserMonadic (..), -- TODO: Can we export ctor?
    ConversionState (..),
    toMonadicRowParser,
  )
where

import Data.Bifunctor (first)
import qualified Data.List as List
import Hpgsql.Encoding (ColumnInfo, RowParser (..))
import qualified Hpgsql.SimpleParser as Parser

-- | Unlike @Hpgsql.Encoding.RowParser@, this has a @Monad@ instance.
-- You should prefer to use @Hpgsql.Encoding.RowParser@ (through @FromPgRow@ instances)
-- instead of this, and use this only if your row decoder is complex enough that
-- decoded fields can change the behaviour of other decoded fields.
-- The regular @RowParser@ can even type-check queries that return no results, while
-- this can't.
newtype RowParserMonadic a = RowParserMonadic
  { -- | Returns the parsed row and the number of columns parsed
    fullRowParser :: ConversionState -> Parser.Parser (a, Int)
  }

newtype ConversionState = ConversionState
  { colsLeftToParse :: [ColumnInfo]
  }

instance Functor RowParserMonadic where
  fmap f (RowParserMonadic {fullRowParser}) = RowParserMonadic $ \cs -> fmap (first f) (fullRowParser cs)

instance Applicative RowParserMonadic where
  pure v = RowParserMonadic $ \_cs -> pure (v, 0)
  RowParserMonadic {fullRowParser = rpF} <*> RowParserMonadic {fullRowParser = rpV} = RowParserMonadic $ \cs -> do
    (rowF, n1) <- rpF cs
    (rowV, n2) <- rpV cs {colsLeftToParse = List.drop n1 cs.colsLeftToParse}
    pure (rowF rowV, n1 + n2)

instance Monad RowParserMonadic where
  RowParserMonadic {fullRowParser} >>= f = RowParserMonadic $ \cs0 -> do
    (row, numColsParsed) <- fullRowParser cs0
    let RowParserMonadic {fullRowParser = parserOfRemainder} = f row
    parserOfRemainder cs0 {colsLeftToParse = List.drop numColsParsed cs0.colsLeftToParse}

-- | Takes an Applicative row parser (which can type-check result rows before even fetching
-- any rows from the response) and transforms it into a Monadic row parser, which has no such
-- type-checking.
toMonadicRowParser :: RowParser a -> RowParserMonadic a
toMonadicRowParser RowParser {fullRowParser, numExpectedColumns} = RowParserMonadic $ \cs -> do
  let numActualCols = length cs.colsLeftToParse
  case compare numActualCols numExpectedColumns of
    EQ -> (,numExpectedColumns) <$> fullRowParser cs.colsLeftToParse
    GT -> (,numExpectedColumns) <$> fullRowParser (List.take numExpectedColumns cs.colsLeftToParse)
    LT -> fail $ "More number of columns expected by the row parser than found in query results. Expected " ++ show numExpectedColumns ++ " but got " ++ show numActualCols
