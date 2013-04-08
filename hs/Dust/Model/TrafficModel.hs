{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.TrafficModel
(
    TrafficModel(..),
    TrafficGenerator(..),
    loadModel,
    makeGenerator
)
where

import Dust.Model.PacketLength
import GHC.Generics
import Data.Serialize
import qualified Data.ByteString as B

data TrafficModel = TrafficModel {
    lengths :: PacketLengthModel
} deriving (Eq, Show, Generic)

data TrafficGenerator = TrafficGenerator {
    generateLength :: IO Int
}

instance Serialize TrafficModel

loadModel :: FilePath -> IO (Either String TrafficModel)
loadModel path = do
    s <- B.readFile path
    return ((decode s)::(Either String TrafficModel))

makeGenerator :: TrafficModel -> TrafficGenerator
makeGenerator model =
    let (PacketLengthModel probs) = lengths model
        cdf = probsToCDF probs
        lengthGen = nextLength cdf
    in TrafficGenerator lengthGen
