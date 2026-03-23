module Database.PostgreSQL.LibPQ (fname, unescapeBytea, Column(..), ExecStatus(..), Format(..), Oid(..), Result(..), Row(..)) where

import Data.ByteString (ByteString)
import HPgsql.TypeInfo (Oid(..))

data ExecStatus = FatalError
  deriving (Eq, Show)

data Result = Result

data Column = Column

data Row = Row

data Format = Text | Binary

fname :: Result -> Column -> IO (Maybe ByteString)
fname _ _ = pure Nothing

unescapeBytea :: ByteString -> IO (Maybe ByteString)
unescapeBytea _ = pure Nothing
