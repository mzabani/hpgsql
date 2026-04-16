module Hpgsql.Locking (getMyWeakThreadId, withMutex, Mutex, WeakThreadId) where

import Control.Concurrent (mkWeakThreadId, myThreadId)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Hpgsql.InternalTypes (Mutex (..), WeakThreadId (..), throwIrrecoverableError)
#if MIN_VERSION_base(4,19,0)
import GHC.Conc.Sync (fromThreadId)
#else
import GHC.Conc.Sync (showThreadId)
#endif

getMyWeakThreadId :: IO WeakThreadId
getMyWeakThreadId = do
  -- We don't keep a reference to `ThreadId` as it can stop threads from getting
  -- runtime exceptions and can prevent dead threads from being garbage-collected.
  -- It's explained somewhere in hackage.
  tid <- myThreadId
  wtid <- mkWeakThreadId tid
#if MIN_VERSION_base(4,19,0)
  pure $ WeakThreadId wtid (fromThreadId tid)
#else
  let tidStr = showThreadId tid
  pure $ WeakThreadId wtid tidStr
#endif

withMutex ::
  Mutex ->
  IO a ->
  IO a
withMutex (Mutex blockedByT) f = do
  thisThreadId <- getMyWeakThreadId
  bracket
    ( STM.atomically $ do
        blockedBy <- STM.readTVar blockedByT
        newSt :: (WeakThreadId, Int) <- case blockedBy of
          Nothing -> pure {- traceShowWith ("Grabbing ",) $ -} (thisThreadId, 1)
          Just (tid, nGrabs) ->
            if tid == thisThreadId
              then pure {- traceShowWith ("Grabbing ",) $ -} (thisThreadId, nGrabs + 1)
              else STM.retry
        STM.writeTVar blockedByT (Just newSt)
    )
    -- Release lock on success or error
    ( const $ void $ STM.atomically $ do
        blockedBy <- STM.readTVar blockedByT
        newSt <- case blockedBy of
          Nothing -> throwIrrecoverableError "Impossible: should have been blocked but was not!"
          Just (tid, nGrabs) ->
            let newLockState = {- traceShowWith ("Releasing ",) $ -} if nGrabs <= 1 then Nothing else Just (thisThreadId, nGrabs - 1)
             in if tid == thisThreadId then pure newLockState else throwIrrecoverableError "Impossible: Lock of a different thread!"
        STM.writeTVar blockedByT newSt
        -- debugPrint "Internal state: [Released control-msg-lock]."
    )
    $ \() -> f
