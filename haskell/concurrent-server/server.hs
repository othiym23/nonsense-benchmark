import Data.ByteString.Char8 as BS hiding (pack, append)
import Data.ByteString.Lazy.Char8 (fromChunks, pack, append)
import Data.ByteString.Lex.Integral (packHexadecimal)
import Data.List

import Control.Monad
import Data.Maybe

import Network hiding (accept)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)
import Control.Concurrent

import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import System.Exit

import Crypto.Hash.SHA256 as Hash

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 1337
    tasks <- getNumCapabilities
    spawn 3 [] $ loop sock

spawn :: Int -> [ProcessID] -> IO () -> IO ()
spawn n pids task
  | n > 0 =
      do pid <- forkProcess task
         spawn (n-1) (pid : pids) task
  | otherwise =
      do installHandler keyboardSignal (Catch (killAll pids)) Nothing
         task

killAll (pid : rest) = do signalProcess killProcess pid
                          killAll rest
killAll [] = exitImmediately ExitSuccess

loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c ack
               string <- recv c 1024
               let nonce = findNonce string
               let response = append (fromChunks [string]) $ append separator (fromChunks [nonce])
               sendAll c response
               sClose c

ack = pack "ok\n"
separator = pack ":"

findNonce :: ByteString -> ByteString
findNonce string = fromMaybe empty $ Data.List.find checkNonce possibleNonces
  where checkNonce :: ByteString -> Bool
        checkNonce nonce = suffix == BS.last (computeSha nonce)

        computeSha nonce = sha
          where sha = Hash.finalize context
                context = Hash.update (Hash.update (Hash.init) string) nonce

possibleNonces = Data.List.map hexify [0..]
 where hexify i = fromMaybe empty $ packHexadecimal i

suffix = '\0'
