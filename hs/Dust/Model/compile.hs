import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import System.IO.Error
import Data.Serialize
import Text.CSV
import Data.List as L

import Dust.Model.TrafficModel
import Dust.Model.PacketLength
import Dust.Model.Content

main = do
    args <- getArgs

    case args of
        (lengthpath:contentpath:modelpath:_) -> compile lengthpath contentpath modelpath
        otherwise      -> putStrLn "Usage: compile [lengths-file] [content-file] [model-file]"

compile :: FilePath -> FilePath -> FilePath -> IO()
compile lengthpath contentPath modelpath  = do
    result <- parseCSVFromFile lengthpath
    case result of
        Left error -> putStrLn "Error parsing CSV"
        Right contents -> do
            let lengthModel = PacketLengthModel $ process contents
            contentModel <- loadContentModel contentPath
            let model = TrafficModel lengthModel contentModel
            B.writeFile modelpath (encode model)

process :: CSV -> [Double]
process [] = []
process ((countStr:_):rest) =
    let count = (read countStr) :: Double
    in [count] ++ process rest
