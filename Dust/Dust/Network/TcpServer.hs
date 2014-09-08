module Dust.Network.TcpServer
(
 server
) where

import Network (listenOn, PortID(PortNumber))
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL
import Network.Socket.ByteString (sendAll)
import Data.ByteString.Lazy (ByteString, fromChunks)
import Control.Monad (forever)

import Dust.Network.Util

type Host = SockAddr

server :: String -> PortNumber -> (Socket -> IO()) -> IO()
server host port handleRequest = withSocketsDo $ do
        sock <- initSocket host port
        forever $ acceptAndProcess sock handleRequest
        sClose sock

initSocket :: String -> PortNumber -> IO(Socket)
initSocket host port = listenOn $ PortNumber port

acceptAndProcess :: Socket -> (Socket-> IO()) -> IO()
acceptAndProcess sock handleRequest = do
    (s, _) <- accept sock
    setSocketOption s NoDelay 1
    process handleRequest s

process :: (Socket -> IO()) -> Socket -> IO()
process handleRequest sock = do
        handleRequest sock
        sClose sock
        return ()
