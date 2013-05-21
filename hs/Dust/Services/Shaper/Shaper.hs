module Dust.Services.Shaper.Shaper
(
    getShapedBytes,
    putBytes
)
where

import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy
import Control.Exception

import Dust.Network.Util
import Dust.Model.TrafficModel

getShapedBytes :: Socket -> IO()
getShapedBytes sock = do
    putStrLn "getting"
    bsTry <- try (recv sock 4096) :: IO (Either IOError B.ByteString)
    case bsTry of
        Left error -> do
            putStrLn "Socket closed"
            putStrLn "Closing socket"
            sClose sock
        Right bs -> do
            putStrLn $ "got " ++ (show (B.length bs))
            getShapedBytes sock

putBytes :: TrafficGenerator -> Socket -> IO()
putBytes gen sock = do
    putStrLn "putting"
    len <- generateLength gen
    putStrLn $ (show len) ++ " to write"
    bytes <- getEntropy len
    sendTry <- try (sendAll sock bytes) :: IO (Either IOError ())
    case sendTry of
        Left error -> do
            putStrLn "Socket closed"
            putStrLn "Closing socket"
            sClose sock
        Right _ -> do
            putStrLn $ "sent " ++ (show (B.length bytes))
            putBytes gen sock
