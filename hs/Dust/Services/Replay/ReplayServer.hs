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
import System.Environment (getArgs)

import Dust.Model.Port
import Dust.Model.Packet
import Dust.Model.TrafficModel
import qualified Dust.Network.TcpServer as TCP
import qualified Dust.Network.UdpServer as UDP
import Dust.Model.Observations (loadObservations, makeModel)
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
            Right stream -> replayServer stream mask
        (pspath:_) -> do
          bs <- B.readFile pspath
          let eitherStream = (decode bs)::(Either String Stream)
          case eitherStream of
            Left error -> putStrLn "Could not load packetstream file"
            Right stream -> replayServer stream (PacketMask [])
        otherwise      -> putStrLn "Usage: replay-server [packetstream-file] <mask-file>"

replayServer :: Stream -> PacketMask -> IO()
replayServer stream@(Stream protocol rport packets) mask = do
    let host = "166.78.129.122"
    let port = 1195 :: PortNumber

    case protocol of
      ProtocolTCP -> do
          let config = TCPConfig (PortNum rport) True          
          TCP.server host port (replayStream config stream mask)
      ProtocolUDP -> do
          let config = UDPConfig (PortNum rport) True host True
          UDP.server host port (replayStream config stream mask)
