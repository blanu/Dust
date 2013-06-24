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
        (modelpath:csvpath:_) -> export modelpath csvpath
        otherwise      -> putStrLn "Usage: export [observation-file] [csv-file]"

export :: FilePath -> FilePath -> IO()
export modelpath csvpath = do
    eitherObs <- loadObservations modelpath
    case eitherObs of
      Left error -> putStrLn "Could not load observations"
      Right obs -> do
        let model = makeModel obs
        let text = csvify modelpath model
        writeFile csvpath text

csvify :: String -> TrafficModel -> String
csvify name (TrafficModel (PacketLengthModel ls) _ _) =
  name ++ "\n" ++ (unlines $ map show ls)

