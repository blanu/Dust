{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.TrafficModel
(
    TrafficModel(..),
    TrafficGenerator(..),
    loadModel,
    makeGenerator
)
where

import GHC.Generics
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Dust.Model.PacketLength
import qualified Dust.Model.Content as C
import Dust.Model.Port

data TrafficModel = TrafficModel {
    lengths :: PacketLengthModel,
    content :: C.ContentModel,
    ports   :: PortModel
} deriving (Generic)
instance Serialize TrafficModel

data TrafficGenerator = TrafficGenerator {
    generateLength :: IO Int,
    encodeContent :: (ByteString -> ByteString),
    decodeContent :: (ByteString -> ByteString)
}

loadModel :: FilePath -> IO (Either String TrafficModel)
loadModel path = do
    s <- B.readFile path
    return ((decode s)::(Either String TrafficModel))

makeGenerator :: TrafficModel -> TrafficGenerator
makeGenerator model =
    let (PacketLengthModel probs) = lengths model
        cdf = probsToCDF probs
        lengthGen = nextLength cdf
        (C.ContentModel tree) = content model
        enc = C.encodeContent tree
        dec = C.decodeContent tree
    in TrafficGenerator lengthGen enc dec
