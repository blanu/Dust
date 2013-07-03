import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import System.IO.Error
import Data.Serialize
import Text.CSV
import Data.List as L
import Network.Pcap
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad
import System.Directory

import Dust.Model.TrafficModel
import Dust.Model.PacketLength
import Dust.Model.Content
import Dust.Model.Observations
import Dust.Model.Packet

main = do
    args <- getArgs

    case args of
        ("--live":device:countStr:port:modelpath:_) -> do
            let count = (read countStr)::(Int)
            updateLive device count port modelpath
        (pcappath:port:modelpath:_) -> updateOffline pcappath port modelpath
        otherwise      -> putStrLn "Usage: update [pcap-file | --live device count] [port] [observation-file]"

updateOffline :: FilePath -> String -> FilePath -> IO()
updateOffline pcappath port modelpath = do
    pcap <- openOffline pcappath
    putStrLn "Opened pcap file"
    let ipmask = (fromIntegral 0)::Word32
    setFilter pcap ("tcp port " ++ port) True ipmask
    putStrLn $ "Set filter to port " ++ port

    let iport = (read port)::Int
    obs <- ensureObservations modelpath
    let obs' = observePort obs iport

    obs'' <- processOffline True pcap obs'
    saveObservations modelpath obs''

updateLive :: String -> Int -> String -> FilePath -> IO()
updateLive device count port modelpath = do
    pcap <- openLive device 1500 False (1000) -- 1 second
    putStrLn $ "Opened " ++ device ++ " in live capture mode"
    let ipmask = (fromIntegral 0)::Word32
    setFilter pcap ("tcp port " ++ port) True ipmask
    putStrLn $ "Set filter to port " ++ port

    let iport = (read port)::Int
    obs <- ensureObservations modelpath
    let obs' = observePort obs iport

    obs'' <- processLive True pcap obs' count
    saveObservations modelpath obs''

processLive :: Bool -> PcapHandle -> Observations -> Int -> IO Observations
processLive first pcap obs 0 = return obs
processLive first pcap obs count = do
    (hdr, body) <- nextBS pcap
    if hdrWireLength hdr /= 0
        then do
            let eitherPacket = parsePacket body
            case eitherPacket of
              Left error -> do
                putStrLn $ "Error parsing packet"
                processLive first pcap obs count
              Right (Packet _ _ _ payload) -> do
                let obs' = observePacket obs payload
                let obs'' = if first then (observeSubstrings obs payload) else obs'
                processLive False pcap obs'' (count-1)
        else return obs

processOffline :: Bool -> PcapHandle -> Observations -> IO Observations
processOffline first pcap obs = do
    (hdr, body) <- nextBS pcap
    if hdrWireLength hdr /= 0
        then do
            let eitherPacket = parsePacket body
            case eitherPacket of
              Left error -> do
                putStrLn $ "Error parsing packet"
                processOffline first pcap obs
              Right (Packet _ _ _ payload) -> do
                let obs' = observePacket obs payload
                let obs'' = observeSubstrings obs payload
                processOffline False pcap obs''
        else return obs
