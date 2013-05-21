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
import Dust.Services.Shaper.Shaper

main :: IO()
main = do
    eitherModel <- loadModel "traffic.model"
    case eitherModel of
        Left error -> putStrLn "Error loading model"
        Right model -> do
            let gen  = makeGenerator model
            shaperClient gen

shaperClient :: TrafficGenerator -> IO()
shaperClient gen = do
    let host = "166.78.129.122"
    let port = 6995

    client host port (shape gen)

client :: String -> PortNumber -> (Socket -> IO()) -> IO()
client host port handleRequest = withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr host
        connect sock (SockAddrInet port addr)
        setSocketOption sock NoDelay 1

        handleRequest sock

shape :: TrafficGenerator -> Socket -> IO()
shape gen sock = do
    putStrLn "Shaping..."
    forkIO $ putBytes gen sock
    getShapedBytes sock
