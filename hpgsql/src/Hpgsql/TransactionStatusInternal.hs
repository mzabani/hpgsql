module Hpgsql.TransactionStatusInternal (TransactionStatus (..)) where

data TransactionStatus
  = -- | A command is in progress
    TransActive
  | -- | Not inside a transaction
    TransIdle
  | -- | Inside a transaction, but no command running
    TransInTrans
  | TransInError
  deriving stock (Eq, Show)
