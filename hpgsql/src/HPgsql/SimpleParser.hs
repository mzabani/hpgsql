-- |
-- A minimal attoparsec-like parser that uses CPS to reduce allocations
-- and perform better than attoparsec, at least the way we use it in
-- hpgsql.
--
-- In benchmarks, this can improve performance by 12-15% materializing
-- query results.
module HPgsql.SimpleParser
  ( Parser (..),
    ParseResult (..),
    parseOnly,
    take,
    endOfInput,
    match,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Prelude hiding (take)

data ParseResult a
  = ParseFail !String
  | ParseOk !a
  deriving stock (Show)

-- | A parser that consumes a strict 'ByteString'.
newtype Parser a = Parser
  { unParser ::
      forall r.
      ByteString ->
      (String -> r) ->
      -- \^ failure continuation
      (a -> ByteString -> r) ->
      -- \^ success continuation
      r
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \bs kf ks ->
    p bs kf (\a bs' -> ks (f a) bs')
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \bs _ ks -> ks a bs
  {-# INLINE pure #-}

  Parser pf <*> Parser pa = Parser $ \bs kf ks ->
    pf bs kf (\f bs' -> pa bs' kf (\a bs'' -> ks (f a) bs''))
  {-# INLINE (<*>) #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}

  Parser p >>= k = Parser $ \bs kf ks ->
    p bs kf (\a bs' -> unParser (k a) bs' kf ks)
  {-# INLINE (>>=) #-}

instance MonadFail Parser where
  fail msg = Parser $ \_ kf _ -> kf msg
  {-# INLINE fail #-}

-- | Run a parser and return either an error message or the parsed value,
-- using the strict 'ParseResult' type. Any unconsumed trailing input is
-- discarded.
parseOnly :: Parser a -> ByteString -> ParseResult a
parseOnly (Parser p) bs = p bs ParseFail (\a _ -> ParseOk a)
{-# INLINE parseOnly #-}

-- | Consume exactly @n@ bytes of input, failing if fewer than @n@ bytes
-- remain.
take :: Int -> Parser ByteString
take n = Parser $ \bs kf ks ->
  -- Special-casing n>0 helps reduce memory usage
  -- by ~1.5% in our benchmarks without a measurable
  -- difference in run time
  if n > 0
    then
      if BS.length bs >= n
        then case BS.splitAt n bs of
          (!h, !t) -> ks h t
        else kf ("take: wanted " <> show n <> " bytes but only " <> show (BS.length bs) <> " remain")
    else
      ks mempty bs
{-# INLINE take #-}

-- | Succeeds only when the input has been fully consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \bs kf ks ->
  if BS.null bs then ks () bs else kf "endOfInput: input remaining"
{-# INLINE endOfInput #-}

-- | Run a parser and additionally return the slice of input it consumed.
-- Because the input is a strict 'ByteString', the returned slice is a view
-- over the original buffer and allocates no extra memory.
match :: Parser a -> Parser (ByteString, a)
match (Parser p) = Parser $ \bs kf ks ->
  p
    bs
    kf
    ( \a bs' ->
        let !consumed = BS.take (BS.length bs - BS.length bs') bs
         in ks (consumed, a) bs'
    )
{-# INLINE match #-}
