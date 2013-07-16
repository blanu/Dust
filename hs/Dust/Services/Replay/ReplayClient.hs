import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket hiding (recv, Stream)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy
import Control.Exception
import Data.Word (Word16)
import Network.Pcap
import System.Environment (getArgs)

import Dust.Network.Util
import Dust.Model.Packet
import Dust.Model.TrafficModel
import Dust.Model.Observations
import Dust.Services.Replay.Replay

main :: IO()
main = do
    args <- getArgs

    case args of
        (pspath:maskfile:_) -> do
          mask <- loadMask maskfile
          bs <- B.readFile pspath
          let eitherStream = (decode bs)::(Either String Stream)
          case eitherStream of
            Left error -> putStrLn "Could not load packetstream file"
            Right stream -> replayClient stream mask
        (pspath:_) -> do
          bs <- B.readFile pspath
          let eitherStream = (decode bs)::(Either String Stream)
          case eitherStream of
            Left error -> putStrLn "Could not load packetstream file"
            Right stream -> replayClient stream (PacketMask [])
        otherwise      -> putStrLn "Usage: replay-client [packetstream-file] <mask-file>"

replayClient :: Stream -> PacketMask -> IO()
replayClient stream@(Stream protocol rport packets) mask = do
    let host = "166.78.129.122"
    let port = PortNum 2013

    case protocol of
      ProtocolTCP -> do
          let config = TCPConfig (PortNum rport) False          
          client protocol host port (replayStream config stream mask)
      ProtocolUDP -> do
          let config = UDPConfig (PortNum rport) False host True
          client protocol host port (replayStream config stream mask)

client :: Protocol -> String -> PortNumber -> (Socket -> IO()) -> IO()
client protocol host port handleRequest = withSocketsDo $ do
    case protocol of
      ProtocolTCP -> do
        sock <- socket AF_INET NS.Stream defaultProtocol
        addr <- inet_addr host
        connect sock (SockAddrInet port addr)
        setSocketOption sock NoDelay 1

        handleRequest sock

      ProtocolUDP -> do
        let iport = 2014
        let myport = PortNum iport
        sock <- socket AF_INET Datagram defaultProtocol
        addr <- inet_addr host
        putStrLn $ "Binding to " ++ (show iport)
        bindSocket sock (SockAddrInet myport iNADDR_ANY)
        connect sock (SockAddrInet port addr)

        handleRequest sock
