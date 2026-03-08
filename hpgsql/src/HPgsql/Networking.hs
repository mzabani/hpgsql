module HPgsql.Networking
  ( recvNonBlocking,
    socketWaitRead,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Exception.Safe (throw)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import Foreign (Ptr, castPtr)
import Foreign.C (CChar (..), CInt (..), CSize (..), eAGAIN, eWOULDBLOCK, getErrno)
import Network.Socket (Socket, withFdSocket)

socketWaitRead :: Socket -> IO ()
socketWaitRead socket = withFdSocket socket (threadWaitRead . fromIntegral)

recvNonBlocking :: Socket -> CSize -> IO ByteString
recvNonBlocking s nbytes = withFdSocket s $ \fd -> createAndTrim (fromIntegral nbytes) $ \buffer -> do
  -- Largely copied from https://hackage-content.haskell.org/package/network-3.2.8.0/docs/src/Network.Socket.Buffer.html#recvBufNoWait and other functions from the network library,
  -- but then modified to our needs.
  r <- c_recv fd (castPtr buffer) nbytes 0 {-flags-}
  if r >= 0
    then do
      -- putStrLn $ "Asked for " ++ show nbytes ++ ", got " ++ show r
      pure $ fromIntegral r
    else do
      err <- getErrno
      if err == eAGAIN || err == eWOULDBLOCK
        then do
          putStrLn $ "Asked for " ++ show nbytes ++ ", but got eAGAIN"
          pure 0
        else
          throw $ userError "Internal error in hpgsql's recvNonBlocking"

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
