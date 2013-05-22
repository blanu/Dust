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

import Dust.Model.TrafficModel
import Dust.Model.PacketLength
import Dust.Model.Content

main = do
    args <- getArgs

    case args of
        (pcappath:port:modelpath:_) -> update pcappath port modelpath
        otherwise      -> putStrLn "Usage: update [pcap-file] [port] [model-file]"

update :: FilePath -> String -> FilePath -> IO()
update pcappath port modelpath = do
    pcap <- openOffline pcappath
    putStrLn "Opened pcap file"
    let ipmask = (fromIntegral 0)::Word32
    setFilter pcap ("tcp port " ++ port) True ipmask
    putStrLn $ "Set filter to port " ++ port
    

callback :: PktHdr -> Ptr Word8 -> IO ()
callback hdr payload = do
    let bs = toBS payload
    return ()

--    result <- parseCSVFromFile lengthpath
    return ()
--    case result of
  --      Left error -> putStrLn "Error parsing CSV"
    --    Right contents -> do
      --      let lengthModel = PacketLengthModel $ process contents
        --    contentModel <- loadContentModel contentPath
          --  let model = TrafficModel lengthModel contentModel
            --B.writeFile modelpath (encode model)

process :: CSV -> [Double]
process [] = []
process ((countStr:_):rest) =
    let count = (read countStr) :: Double
    in [count] ++ process rest
