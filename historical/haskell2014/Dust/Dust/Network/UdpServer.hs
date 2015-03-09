module Dust.Network.UdpServer
(
 server
) where

import Network (PortID(PortNumber))
import Network.Socket
import Control.Monad (forever)

server :: String -> PortNumber -> (Socket -> IO()) -> IO()
server host port@(PortNum iport) handleRequest = withSocketsDo $ do
        sock <- socket AF_INET Datagram defaultProtocol
        putStrLn $ "Binding to " ++ (show iport)
        bindSocket sock (SockAddrInet port iNADDR_ANY)
        handleRequest sock
