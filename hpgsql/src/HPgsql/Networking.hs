module HPgsql.Networking
  ( recvNonBlocking,
    socketWaitRead,
    socketWaitWrite,
    sendNonBlocking,
  )
where

-- \|
-- The code in this module is 99.9% a copy from the `network` library, with all the small
-- occurrences of blocking code found there removed (mostly threadWaitWrite and threadWaitRead,
-- directly or indirectly called by other functions).
-- Ideally we would ask the developer of `network` to expose these functions: it seems like it might be
-- possible without much hassle.

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Exception.Safe (throw)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Foreign (Ptr, Storable (..), Word8, allocaArray, castPtr, nullPtr, plusPtr)
import Foreign.C (CChar (..), CInt (..), CSize (..), eAGAIN, eWOULDBLOCK, getErrno)
import Network.Socket (Socket, withFdSocket)
import System.Posix.Types (CSsize (..))

socketWaitRead :: Socket -> IO ()
socketWaitRead socket = withFdSocket socket (threadWaitRead . fromIntegral)

socketWaitWrite :: Socket -> IO ()
socketWaitWrite socket = withFdSocket socket (threadWaitWrite . fromIntegral)

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

sendNonBlocking :: Socket -> L.ByteString -> IO Int64
sendNonBlocking s lbs = do
  -- Largely copied from https://hackage-content.haskell.org/package/network-3.2.8.0/docs/src/Network.Socket.ByteString.Lazy.Posix.html#send,
  -- but then modified to our needs.
  let cs = take maxNumChunks (L.toChunks lbs)
      len = length cs
  siz <- withFdSocket s $ \fd -> allocaArray len $ \ptr ->
    withPokes cs ptr $ \niovs ->
      -- This part has `throwSocketErrorWaitWrite s "writev"` in the
      -- original codebase, but we don't have that here because that
      -- calls threadWaitWrite, which blocks.
      c_writev fd ptr niovs
  return $ fromIntegral siz
  where
    withPokes ss p f = loop ss p 0 0
      where
        loop (c : cs) q k niovs
          | k < maxNumBytes = unsafeUseAsCStringLen c $ \(ptr, len) -> do
              poke q $ IOVec (castPtr ptr) (fromIntegral len)
              loop
                cs
                (q `plusPtr` sizeOf (IOVec nullPtr 0))
                (k + fromIntegral len)
                (niovs + 1)
          | otherwise = f niovs
        loop _ _ _ niovs = f niovs
    maxNumBytes = 4194304 :: Int -- maximum number of bytes to transmit in one system call
    maxNumChunks = 1024 :: Int -- maximum number of chunks to transmit in one system call

data IOVec = IOVec
  { iovBase :: Ptr Word8,
    iovLen :: CSize
  }

instance Storable IOVec where
  sizeOf ~_ = (16)
  alignment ~_ = alignment (0 :: CInt)

  peek p = do
    base <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
    len <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
    return $ IOVec base len

  poke p iov = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (iovBase iov)
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (iovLen iov)

-- -- | @withIOVec cs f@ executes the computation @f@, passing as argument a pair
-- -- consisting of a pointer to a temporarily allocated array of pointers to
-- -- IOVec made from @cs@ and the number of pointers (@length cs@).
-- -- /Unix only/.
-- withIOVec :: [(Ptr Word8, Int)] -> ((Ptr IOVec, Int) -> IO a) -> IO a
-- withIOVec [] f = f (nullPtr, 0)
-- withIOVec cs f =
--   allocaArray csLen $ \aPtr -> do
--     zipWithM_ pokeIov (ptrs aPtr) cs
--     f (aPtr, csLen)
--   where
--     csLen = length cs
--     ptrs = iterate (`plusPtr` sizeOf (IOVec nullPtr 0))
--     pokeIov ptr (sPtr, sLen) = poke ptr $ IOVec sPtr (fromIntegral sLen)

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize
