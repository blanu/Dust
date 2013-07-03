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
import Data.Word (Word16)
import Network.Pcap
import System.Environment (getArgs)

import Dust.Network.Util
import Dust.Model.TrafficModel
import Dust.Model.Observations
import Dust.Services.Replay.Replay

main :: IO()
main = do
    args <- getArgs

    case args of
        (pcappath:protocol:port:_) -> replayClient pcappath protocol port
        otherwise      -> putStrLn "Usage: replay-client [pcap-file] [tcp|udp] [port]"

replayClient :: FilePath -> String -> String -> IO()
replayClient pcappath protocol rport = do
    let host = "166.78.129.122"
    let port = PortNum 2013

    case protocol of
      "tcp" -> do
          let config = TCPConfig rport False
          pcap <- openPcap pcappath config
          client protocol host port (replayStream config pcap)
      "udp" -> do
          let config = UDPConfig rport False host True
          pcap <- openPcap pcappath config
          client protocol host port (replayStream config pcap)
      otherwise -> putStrLn $ "Unknown protocol " ++ protocol

client :: String -> String -> PortNumber -> (Socket -> IO()) -> IO()
client protocol host port handleRequest = withSocketsDo $ do
    case protocol of
      "tcp" -> do
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr host
        connect sock (SockAddrInet port addr)
        setSocketOption sock NoDelay 1

        handleRequest sock
      "udp" -> do
        let iport = 2014
        let myport = PortNum iport
        sock <- socket AF_INET Datagram defaultProtocol
        addr <- inet_addr host
        putStrLn $ "Binding to " ++ (show iport)
        bindSocket sock (SockAddrInet myport iNADDR_ANY)
        connect sock (SockAddrInet port addr)

        handleRequest sock
      otherwise -> putStrLn $ "Unknown protocol " ++ protocol
