import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import System.IO.Error
import Data.Serialize
import Text.CSV

import Dust.Model.TrafficModel
import Dust.Model.PacketLength

main = do
    args <- getArgs

    case args of
        (lengthpath:modelpath:_) -> compile lengthpath modelpath
        otherwise      -> putStrLn "Usage: compile [lengths-file] [model-file]"

compile :: FilePath -> FilePath -> IO()
compile lengthpath modelpath = do
    result <- parseCSVFromFile lengthpath
    case result of
        Left error -> putStrLn "Error parsing CSV"
        Right contents -> putStrLn $ show $ process contents

process :: CSV -> [(Int,Int)]
process [] = []
process (row:rows) = processRow row ++ process rows

processRow :: Record -> [(Int,Int)]
processRow (indexStr:countStr:_) =
    let index = (read indexStr) :: Int
        count = (read countStr) :: Int
    in [(index,count)]
processRow (_:[]) = []
processRow [] = []

