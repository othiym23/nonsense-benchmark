import Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Integral (packHexadecimal)
import Data.List

import Control.Monad
import Data.Maybe

import Network hiding (accept)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent
import Control.Exception

import Crypto.Hash.SHA256 as Hash

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 1337
    tasks <- getNumCapabilities
    spawn tasks $ loop sock

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

forkAndWait :: IO () -> IO ()
forkAndWait io = do
  mvar <- newEmptyMVar
  forkFinally io (\_ -> putMVar mvar ())
  takeMVar mvar

spawn :: Int -> IO () -> IO ()
spawn n task
  | n > 0 = do forkOS task
               spawn (n-1) task
  | otherwise = forkAndWait task

loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c ack
               string <- recv c 1024
               let nonce = findNonce string
               let response = append string $ append separator nonce
               sendAll c response
               sClose c

ack = pack "ok\n"
separator = pack ":"

findNonce :: ByteString -> ByteString
findNonce string = fromMaybe separator $ Data.List.find checkNonce possibleNonces
  where checkNonce :: ByteString -> Bool
        checkNonce nonce = suffix == BS.last (computeSha nonce)

        computeSha nonce = sha
          where sha = Hash.finalize context
                context = Hash.update (Hash.update (Hash.init) string) nonce


possibleNonces = Data.List.map hexify [0..]
  where hexify i = fromMaybe separator $ packHexadecimal i

suffix = '\0'
