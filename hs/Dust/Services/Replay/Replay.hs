module Dust.Services.Replay.Replay
(
    ReplayConfig(..),
    PacketMask(..),
    loadMask,
    replayStream,
    linger
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
import Network.Socket hiding (recv, sendTo, Stream)
import Network.Socket.ByteString (recv, sendAll, sendTo)
import System.Entropy
import Control.Exception
import Data.Word (Word8, Word16, Word32)
import Data.String
import Data.List.Split
import Control.Monad (forever)

import Dust.Network.Util
import Dust.Model.TrafficModel
import Dust.Model.Packet

data ReplayConfig =
    TCPConfig PortNumber Bool
  | UDPConfig PortNumber Bool String Bool

data PacketMask = PacketMask [[(Int,Word8)]] deriving (Show) -- [[(Offset,Byte)]], list index is packet number

nextMask :: PacketMask -> PacketMask
nextMask (PacketMask []) = PacketMask []
nextMask (PacketMask (mask:masks)) = PacketMask masks

loadMask :: FilePath -> IO PacketMask
loadMask path = do
  file <- readFile path
  return $ parseMasks $ map words (lines file)

parseMasks :: [[String]] -> PacketMask
parseMasks sss = PacketMask $ map parseMask sss

parseMask :: [String] -> [(Int,Word8)]
parseMask ss = map parseByteMask ss

parseByteMask :: String -> (Int,Word8)
parseByteMask s =
  let (a:b:_) = splitOn "," s
      a' = (read a)::Int
      b' = (read b)::Word8
  in (a',b')

replayStream :: ReplayConfig -> Stream -> PacketMask -> Socket -> IO()
replayStream config stream@(Stream _ _ []) mask sock = do
  putStrLn "Done replaying"
  putStrLn "Closing socket"
  case config of
--   (TCPConfig _ _)     -> sClose sock
   (TCPConfig _ _)     -> linger sock
   (UDPConfig _ _ _ _) -> return()
replayStream config stream@(Stream protocol port (packet:packets)) mask sock = do
  replayPacket config packet mask sock
  replayStream (markNotFirst config) (Stream protocol port packets) (nextMask mask) sock

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
packetDirection :: Packet -> PortNumber -> Bool
packetDirection (Packet _ _ tcp _) (PortNum port) = (destport tcp) == port

replayPacket :: ReplayConfig -> Packet -> PacketMask -> Socket -> IO()
replayPacket config packet@(Packet _ _ _ payload) mask sock = do
    case (replayDirection config packet) of
        True  -> getReplayedBytes (B.length payload) sock
        False -> sendReplayedBytes config packet mask sock

linger :: Socket -> IO()
linger = forever $ getReplayedBytes 1024

getReplayedBytes :: Int -> Socket -> IO()
getReplayedBytes count sock = do
    putStrLn $ "waiting for " ++ show count
    bsTry <- try (recv sock count) :: IO (Either IOError B.ByteString)
    case bsTry of
        Left error -> do
            putStrLn $ "Error: " ++ show error
            putStrLn "Closing socket"
--            sClose sock
            return ()
        Right bs -> do
            putStrLn $ "got " ++ (show (B.length bs))
            if (B.length bs) == 0
              then do
                putStrLn "0 length read"
                putStrLn "Closing socket"
--                sClose sock
                return ()
              else if (B.length bs) < count
                then getReplayedBytes (count - (B.length bs)) sock
                else return ()

sendReplayedBytes :: ReplayConfig -> Packet -> PacketMask -> Socket -> IO()
sendReplayedBytes config packet@(Packet _ _ transport payload) mask sock = do
    putStrLn $ "sending " ++ show (B.length payload)
    putStrLn $ show packet
    let maskedPayload = applyMask mask payload
    case config of
        (TCPConfig _ _) -> do
            sendTry <- try (sendAll sock maskedPayload) :: IO (Either IOError ())
            case sendTry of
                Left error -> do
                    putStrLn $ "Error: " ++ show error
                    putStrLn "Closing socket"
--                    sClose sock
                Right _ -> do
                    putStrLn $ "sent " ++ (show (B.length payload))
        (UDPConfig sport dir host first) -> do
            bindAddr <- inet_addr host
            let addr = SockAddrInet (PortNum (if dir then 2014 else 2013)) bindAddr
            sendTry <- try (sendTo sock maskedPayload addr) :: IO (Either IOError Int)
            case sendTry of
                Left error -> do
                    putStrLn $ "Error: " ++ show error
                Right _ -> do
                    putStrLn $ "sent " ++ (show (B.length payload)) ++ " to " ++ (show addr)
    return ()

applyMask :: PacketMask -> ByteString -> ByteString
applyMask (PacketMask []) bs = bs
applyMask (PacketMask (mask:_)) bs = applyByteMasks mask bs

applyByteMasks :: [(Int,Word8)] -> ByteString -> ByteString
applyByteMasks [] bs = bs
applyByteMasks (mask:masks) bs = applyByteMasks masks (applyByteMask mask bs)

applyByteMask :: (Int,Word8) -> ByteString -> ByteString
applyByteMask (offset,value) bs = 
  let front = B.take offset bs
      back = B.drop (offset+1) bs
  in front `B.append` (value `B.cons` back)

