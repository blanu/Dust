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

    obs <- ensureObservations modelpath
    putStrLn "Obs"
    putStrLn $ show obs

    putStrLn "Processing"
    obs' <- processOffline pcap obs
    putStrLn "New observations"
    saveObservations modelpath obs'

updateLive :: String -> Int -> String -> FilePath -> IO()
updateLive device count port modelpath = do
    pcap <- openLive device 1500 False (1000) -- 1 second
    putStrLn $ "Opened " ++ device ++ " in live capture mode"
    let ipmask = (fromIntegral 0)::Word32
    setFilter pcap ("tcp port " ++ port) True ipmask
    putStrLn $ "Set filter to port " ++ port

    obs <- ensureObservations modelpath
    putStrLn "Obs"
    putStrLn $ show obs

    putStrLn "Processing"
    obs' <- processLive pcap obs count
    putStrLn "New observations"
    saveObservations modelpath obs'

processLive :: PcapHandle -> Observations -> Int -> IO Observations
processLive pcap obs 0 = return obs
processLive pcap obs count = do
    (hdr, body) <- nextBS pcap
    if hdrWireLength hdr /= 0
        then do
            putStrLn "Got next"
            let obs' = observePacket obs body
            putStrLn $ show obs'
            processLive pcap obs' (count-1)
        else return obs

processOffline :: PcapHandle -> Observations -> IO Observations
processOffline pcap obs = do
    (hdr, body) <- nextBS pcap
    if hdrWireLength hdr /= 0
        then do
            putStrLn "Got next"
            let obs' = observePacket obs body
            putStrLn $ show obs'
            processOffline pcap obs'
        else return obs

