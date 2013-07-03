import Data.ByteString (ByteString)
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
import Data.Map

import Dust.Model.TrafficModel
import Dust.Model.PacketLength
import Dust.Model.Content
import Dust.Model.Observations

main = do
    args <- getArgs

    case args of
        (modelpath:_) -> showModel modelpath
        otherwise      -> putStrLn "Usage: show [observation-file]"

showModel :: FilePath -> IO()
showModel modelpath = do
    eitherObs <- loadObservations modelpath
    case eitherObs of
      Left error -> putStrLn "Could not load observations"
      Right obs -> do
--        putStrLn $ show obs
        showSubstrings obs

showSubstrings :: Observations -> IO()
showSubstrings (Observations _ _ _ (SubstringObservations items)) = showSubstring 0 items

showSubstring :: Int -> [(Map ByteString Int)] -> IO()
showSubstring offset [] = return ()
showSubstring offset (item:rest) = do
  showMap offset item
  showSubstring (offset+1) rest

showMap :: Int -> (Map ByteString Int) -> IO()
showMap offset item = showGoodItems offset (assocs item)

showGoodItems :: Int -> [(ByteString,Int)] -> IO()
showGoodItems offset [] = return ()
showGoodItems offset ((bs,count):rest) = do
  showGoodItem offset bs count
  showGoodItems offset rest

showGoodItem :: Int -> ByteString -> Int -> IO()
showGoodItem offset bs count = do
  if count > 1
    then putStrLn $ (show offset) ++ " " ++ (show bs) ++ " " ++ (show count)
    else return ()
