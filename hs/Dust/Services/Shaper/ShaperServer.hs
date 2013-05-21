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
import Dust.Services.Shaper.Shaper

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
