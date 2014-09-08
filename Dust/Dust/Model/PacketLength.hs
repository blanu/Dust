{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.PacketLength
(
    PacketLengthModel(..),
    loadLengthModel,
    probsToCDF,
    nextLength
)
where

import System.Random
import GHC.Generics
import Data.Serialize
import Data.Random.Shuffle.Weighted
import Data.Random.RVar
import Data.Random
import Data.Random.Source.IO
import Data.Random.Source.Std
import Data.Map
import qualified Data.ByteString as B

data PacketLengthModel = PacketLengthModel [Double] deriving (Eq, Show, Generic)

instance Serialize PacketLengthModel

loadLengthModel :: FilePath -> IO (Map Double Int)
loadLengthModel path = do
   probs <- loadProbs path
   return $ probsToCDF probs

loadProbs :: FilePath -> IO [Double]
loadProbs path = do
    s <- B.readFile path
    let result = (decode s)::(Either String [Double])
    case result of
        Left error -> return ([])
        Right arr -> return(arr)

probsToCDF :: [Double] -> Map Double Int
probsToCDF probs = cdfMapFromList $ zip probs [1..(length probs)]

nextLength :: Map Double Int -> IO Int
nextLength cdf = do
    let dist = weightedSampleCDF 1 cdf
    arr <- runRVar dist StdRandom :: IO [Int]
    return (head arr)
