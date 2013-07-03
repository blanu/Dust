module Dust.Services.Replay.Replay
(
    ReplayConfig(..),
    openPcap,
    replayStream
)
where

import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket hiding (recv, sendTo)
import Network.Socket.ByteString (recv, sendAll, sendTo)
import System.Entropy
import Control.Exception
import Network.Pcap
import Data.Word (Word16, Word32)

import Dust.Network.Util
import Dust.Model.TrafficModel
import Dust.Model.Packet

data ReplayConfig =
    TCPConfig String Bool
  | UDPConfig String Bool String Bool

openPcap :: FilePath -> ReplayConfig -> IO PcapHandle
openPcap pcappath config = do
    pcap <- openOffline pcappath
    putStrLn "Opened pcap file"
    let ipmask = (fromIntegral 0)::Word32
    case config of
      (TCPConfig port _)     -> setFilter pcap ("tcp port " ++ port) True ipmask
      (UDPConfig port _ _ _) -> setFilter pcap ("udp port " ++ port) True ipmask
    return pcap

replayStream :: ReplayConfig -> PcapHandle -> Socket -> IO()
replayStream config pcap sock = do
    (hdr, body) <- nextBS pcap
    if hdrWireLength hdr /= 0
        then do
            let eitherHeaders = parsePacket body

            case eitherHeaders of
                Left error -> putStrLn "Error parsing [TCP|UDP]/IP headers"
                Right headers@(Packet _ _ _ payload) -> do
	            if (B.length payload) > 0
        	        then do
                	    let replayDir = replayDirection config headers
	                    replayPacket config replayDir headers sock
        		    replayStream (markNotFirst config) pcap sock
        		else replayStream config pcap sock
        else do
            putStrLn "Done replaying"
            putStrLn "Closing socket"
	    sClose sock
	    return ()

markNotFirst :: ReplayConfig -> ReplayConfig
markNotFirst config@(TCPConfig _ _) = config
markNotFirst (UDPConfig a b c _) = UDPConfig a b c False

-- True if packet direction is the same as config direction
-- False if packet direction is not the same as config direction
-- Config Packet Replay
--   T      T      T    -- Server, Packet is Client->Server, Read
--   T      F      F    -- Server, Packet is Client<-Server, Send
--   F      T      F    -- Client, Packet is Client->Server, Send
--   F      F      T    -- Client, Packet is Client<-Server, Read
replayDirection :: ReplayConfig -> Packet -> Bool
replayDirection config packet =
  case config of
    (TCPConfig port dir)     -> (packetDirection packet port) == dir
    (UDPConfig port dir _ _) -> (packetDirection packet port) == dir

-- True if the destination of the packet is the config port
--   Means packet is going to server
-- False if the destination of the packet is any other port
--   Means packet is going to client
packetDirection :: Packet -> String -> Bool
packetDirection (Packet _ _ tcp _) strport =
    let w16port = (read strport)::Word16
    in (destport tcp) == w16port

replayPacket :: ReplayConfig -> Bool -> Packet -> Socket -> IO()
replayPacket config dir packet@(Packet _ _ _ payload) sock = do
    case dir of
        True  -> getReplayedBytes (B.length payload) sock
        False -> sendReplayedBytes config packet sock

getReplayedBytes :: Int -> Socket -> IO()
getReplayedBytes count sock = do
    putStrLn $ "waiting for " ++ show count
    bsTry <- try (recv sock count) :: IO (Either IOError B.ByteString)
    case bsTry of
        Left error -> do
            putStrLn $ "Error: " ++ show error
            putStrLn "Closing socket"
            sClose sock
            return ()
        Right bs -> do
            putStrLn $ "got " ++ (show (B.length bs))
            if (B.length bs) == 0
              then do
                putStrLn "0 length read"
                putStrLn "Closing socket"
                sClose sock
                return ()
              else if (B.length bs) < count
                then getReplayedBytes (count - (B.length bs)) sock
                else return ()

sendReplayedBytes :: ReplayConfig -> Packet -> Socket -> IO()
sendReplayedBytes config packet@(Packet _ _ transport payload) sock = do
    putStrLn $ "sending " ++ show (B.length payload)
    case config of
        (TCPConfig _ _) -> do
            sendTry <- try (sendAll sock payload) :: IO (Either IOError ())
            case sendTry of
                Left error -> do
                    putStrLn $ "Error: " ++ show error
                    putStrLn "Closing socket"
                    sClose sock
                Right _ -> do
                    putStrLn $ "sent " ++ (show (B.length payload))
        (UDPConfig sport dir host first) -> do
            bindAddr <- inet_addr host
            let addr = SockAddrInet (PortNum (if dir then 2014 else 2013)) bindAddr
            sendTry <- try (sendTo sock payload addr) :: IO (Either IOError Int)
            case sendTry of
                Left error -> do
                    putStrLn $ "Error: " ++ show error
                Right _ -> do
                    putStrLn $ "sent " ++ (show (B.length payload))
    return ()
