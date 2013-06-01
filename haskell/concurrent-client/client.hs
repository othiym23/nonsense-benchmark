module Main where

import Prelude hiding (catch, last)

import Control.Concurrent
import Control.Exception (finally, catch, IOException(..))
import Crypto.Hash.SHA256 (hash)
import Crypto.Random (newGenIO, genBytes, SystemRandom(..))
import Data.ByteString.Char8 (pack, unpack, last, ByteString(..))
import Data.Char (ord)
import Network (withSocketsDo, connectTo, PortID(..))
import System.IO
import Text.Printf (printf)
import Text.Regex (mkRegex, splitRegex)

hashed :: [String] -> ByteString
hashed = hash . pack . concat

hashToStr :: ByteString -> String
hashToStr bytes = unpack bytes >>= printf "%02x"

randomHash :: IO ByteString
randomHash = do
    generator <- newGenIO :: IO SystemRandom
    okay $ genBytes 32 generator
    where okay (Left e)           = return (hashed ["oh", "well"])
          okay (Right (bytes, _)) = return bytes

requestProof :: IO ByteString -> IO ByteString
requestProof hash = withSocketsDo $ do
    handle <- newHandle
    -- putStrLn "Connected to host."
    hSetBuffering handle (BlockBuffering Nothing)

    handshake <- hGetLine handle
    okay handle handshake
    where newHandle        = connectTo "127.0.0.1" (PortNumber 1337)
          okay h "ok"      = sendHash h hash
          okay h handshake = do
              putStrLn $ "Got " ++ handshake ++ " instead of 'ok'."
              requestProof randomHash

sendHash :: Handle -> IO ByteString -> IO ByteString
sendHash handle hash = do
    hashStr <- hash >>= return . hashToStr
    -- putStrLn $ "Sending " ++ hashStr

    hPutStr handle hashStr; hFlush handle
    response <- hGetContents handle
    -- putStrLn $ "Response is " ++ response

    validateResponse hashStr $ return response

validateResponse :: String -> IO String -> IO ByteString
validateResponse sent response = do
    let colonoscopy = splitRegex (mkRegex ":")
    parts <- response >>= return . colonoscopy
    let returned = head parts
    if sent == returned then
        verifyProof parts
    else do
        putStrLn $ "input " ++ sent ++ " !== " ++ returned
        requestProof randomHash

verifyProof :: [String] -> IO ByteString
verifyProof parts = do
    -- putStrLn $ "Next is " ++ (hashToStr next)
    if valid next then
        requestProof $ return next
    else do
        putStrLn $ (concat parts) ++ " did not validate (got" ++ (hashToStr next) ++ " instead)"
        requestProof $ return next
    where next            = hashed parts
          lastChar        = ord . last
          valid candidate = lastChar candidate == 0

spawn :: IO ()
spawn = do
    myID <- myThreadId
    putStrLn $ "Spawning proof requestor on thread " ++ (show myID)
    requestProof randomHash
    return ()

main = go 0
    where
        go :: Int -> IO ()
        go i | i < 64 = do
                   mainTID <- myThreadId
                   myID <- forkIO $ spawn `catch` \e -> (throwTo mainTID (e :: IOException))
                   -- putStrLn $ show myID
                   go (i + 1)
             | otherwise = spawn
