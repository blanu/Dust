module Dust.Network.TcpClient
(
 client
) where

import Network (connectTo, PortID(PortNumber))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Data.ByteString (ByteString)
import Control.Monad (forever)
import Control.Concurrent

import Dust.Network.Util

client :: String -> PortNumber -> ByteString -> IO(ByteString)
client host port msg = withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr host
        connect sock (SockAddrInet port addr)
        sendAll sock msg
        result <- recvAll sock
        sClose sock
        return(result)
