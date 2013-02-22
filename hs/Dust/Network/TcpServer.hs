module Dust.Network.TcpServer
(
 server
) where

import Network (listenOn, PortID(PortNumber))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Data.ByteString (ByteString)
import Control.Monad (forever)

import Dust.Network.Util

type Host = SockAddr

server :: String -> PortNumber -> (ByteString -> IO(ByteString)) -> IO()
server host port transform = withSocketsDo $ do
        sock <- initSocket host port
        forever $ acceptAndProcess sock transform
        sClose sock

initSocket host port = do
        sock <- listenOn $ PortNumber port
        return sock

acceptAndProcess :: Socket -> (ByteString -> IO(ByteString)) -> IO()
acceptAndProcess sock processor = do
    (s, _) <- accept sock
    process processor s

process :: (ByteString -> IO(ByteString)) -> Socket -> IO()
process transform sock = do
        input <- recvAll sock
        output <- transform input
        sendAll sock output
        return ()
