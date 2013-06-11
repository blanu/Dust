import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import System.IO.Error
import Data.Serialize
import Text.CSV
import Data.List as L

import Dust.Model.Observations
import Dust.Model.TrafficModel

main = do
    args <- getArgs

    case args of
        (obspath:modelpath:_) -> compile obspath modelpath
        otherwise      -> putStrLn "Usage: compile [observation-file] [model-file]"

compile :: FilePath -> FilePath -> IO()
compile obspath modelpath  = do
    eitherObs <- loadObservations obspath
    case eitherObs of
        Left error -> putStrLn "Error loading observations"
        Right obs -> do
            let model = makeModel obs
            B.writeFile modelpath (encode model)
