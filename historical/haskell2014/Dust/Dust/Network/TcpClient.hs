module Dust.Network.TcpClient
(
 client
) where

import Network.Socket
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Dust.Network.Util
import Dust.Crypto.Cipher

client :: String -> PortNumber -> (Socket -> IO(Plaintext)) -> IO(Plaintext)
client host port handleRequest = withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr host
        connect sock (SockAddrInet port addr)
        setSocketOption sock NoDelay 1

        handleRequest sock
