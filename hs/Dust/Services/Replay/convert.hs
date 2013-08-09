import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket hiding (recv, Stream)
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy
import Control.Exception
import Data.Word (Word16)
import Network.Pcap
import System.Environment (getArgs)
import Data.Word (Word32)

import Dust.Network.Util
import Dust.Model.Packet
import Dust.Model.TrafficModel
import Dust.Model.Observations
import Dust.Services.Replay.Replay

main :: IO()
main = do
    args <- getArgs

    case args of
        (pcappath:protocol:port:pspath:_) -> convert pcappath protocol port pspath
        otherwise                         -> do
          putStrLn "replay-convert converts .pcap files into the packetstream format for use in replay-server and replay-client"
          putStrLn "Usage: replay-client [pcap-file] [tcp|udp] [port] [packetstream-file]"

convert :: FilePath -> String -> String -> FilePath -> IO()
convert pcappath protocol sport pspath = do
    let rport = (read sport)::Word16
    case protocol of
      "tcp" -> do
          let config = TCPConfig (PortNum rport) False
          pcap <- openPcap pcappath config
          stream <- convertStream config pcap $ Stream ProtocolTCP rport []
          putStrLn "---------------------------"
          putStrLn $ show stream
          let bs = encode stream
          B.writeFile pspath bs
      "udp" -> do
          let config = UDPConfig (PortNum rport) False "" True
          pcap <- openPcap pcappath config
          stream <- convertStream config pcap $ Stream ProtocolUDP rport []
          putStrLn $ show stream
          let bs = encode stream
          B.writeFile pspath bs
      otherwise -> putStrLn $ "Unknown protocol " ++ protocol

convertStream :: ReplayConfig -> PcapHandle -> Stream -> IO Stream
convertStream config pcap stream@(Stream protocol rport packets) = do
    (hdr, body) <- nextBS pcap
    if hdrWireLength hdr /= 0
        then do
            let eitherHeaders = parsePacket body

            case eitherHeaders of
                Left error -> do
                  putStrLn "Error parsing [TCP|UDP]/IP headers"
                  return stream
                Right packet@(Packet _ _ _ payload) -> do
                    putStrLn $ show packet
	            if (B.length payload) > 0
        	        then convertStream config pcap $ Stream protocol rport $ packets ++ [packet]
        		else convertStream config pcap stream
        else return stream

openPcap :: FilePath -> ReplayConfig -> IO PcapHandle
openPcap pcappath config = do
    pcap <- openOffline pcappath
    putStrLn "Opened pcap file"
    let ipmask = (fromIntegral 0)::Word32
    case config of
      (TCPConfig (PortNum port) _)     -> setFilter pcap ("tcp port " ++ (show port)) True ipmask
      (UDPConfig (PortNum port) _ _ _) -> setFilter pcap ("udp port " ++ (show port)) True ipmask
    return pcap

