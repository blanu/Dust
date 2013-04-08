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
        Right contents -> do
            let model = TrafficModel $ PacketLengthModel $ process contents
            putStrLn $ show model
            B.writeFile modelpath (encode model)

process :: CSV -> [Double]
process [] = []
process ((countStr:_):rest) =
    let count = (read countStr) :: Double
    in [count] ++ process rest
