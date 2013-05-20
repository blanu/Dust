import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy

import Dust.Model.TrafficModel
import Dust.Network.TcpServer

main :: IO()
main = do
    eitherModel <- loadModel "traffic.model"
    case eitherModel of
        Left error -> putStrLn "Error loading model"
        Right model -> do
            let gen  = makeGenerator model
            shaperServer gen

shaperServer :: TrafficGenerator -> IO()
shaperServer gen = do
    let host = "0.0.0.0"
    let port = 6995

    server host port (shape gen)

shape :: TrafficGenerator -> Socket -> IO()
shape gen sock = do
    putStrLn "Shaping..."
    forkIO $ getShapedBytes sock
    putBytes gen sock

getShapedBytes :: Socket -> IO()
getShapedBytes sock = do
    putStrLn "getting"
    bs <- recv sock 4096
    putStrLn $ "got " ++ (show (B.length bs))
    getShapedBytes sock

putBytes :: TrafficGenerator -> Socket -> IO()
putBytes gen sock = do
    putStrLn "putting"
    len <- generateLength gen
    putStrLn $ (show len) ++ " to write"
    bytes <- getEntropy len
    sendAll sock bytes
