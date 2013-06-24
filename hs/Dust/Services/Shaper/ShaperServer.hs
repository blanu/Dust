import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket (Socket, PortNumber(..))
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy
import Data.Word (Word16)

import Dust.Model.Port
import Dust.Model.TrafficModel
import Dust.Network.TcpServer
import Dust.Model.Observations (loadObservations, makeModel)
import Dust.Services.Shaper.Shaper

main :: IO()
main = do
    eitherObs <- loadObservations "traffic.model"
    case eitherObs of
        Left error -> putStrLn "Error loading model"
        Right obs -> do
            let model = makeModel obs
            let gen  = makeGenerator model
            let (PortModel portList) = ports model

            forkServers portList $ shaperServer gen

forkServers :: [Int] -> (Int -> IO()) -> IO()
forkServers [] server = return ()
forkServers (port:[]) server = server port
forkServers (port:ports) server = do
    forkIO $ server port
    forkServers ports server

shaperServer :: TrafficGenerator -> Int -> IO()
shaperServer gen iport = do
    let host = "0.0.0.0"
    let port = PortNum $ ((fromIntegral iport)::Word16)

    server host port (shape gen)

shape :: TrafficGenerator -> Socket -> IO()
shape gen sock = do
    putStrLn "Shaping..."
    forkIO $ getShapedBytes sock
    putBytes gen sock
