{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.Observations
(
    Observations(..),
    emptyObservations,
    loadObservations,
    makeModel
)
where

import GHC.Generics
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int
import Data.Word

import Dust.Model.PacketLength
import qualified Dust.Model.Content as C
import Dust.Model.TrafficModel

data Observations = Observations {
    lengths :: LengthObservations,
    content :: ContentObservations
} deriving (Generic)

data LengthObservations = LengthObservations [(Int16, Integer)] deriving Generic
data ContentObservations = ContentObservations [(Word8, Integer)] deriving Generic

emptyObservations :: Observations
emptyObservations = Observations emptyLengthObservations emptyContentObservations

emptyLengthObservations :: LengthObservations
emptyLengthObservations =
    let lengths = [1..1448]::[Int16]
        counts  = take 1448 $ repeat 0
     in LengthObservations $ zip lengths counts

emptyContentObservations :: ContentObservations
emptyContentObservations =
    let lengths = [0..255]::[Word8]
        counts  = take 256 $ repeat 0
     in ContentObservations $ zip lengths counts

loadObservations :: FilePath -> IO (Either String Observations)
loadObservations path = do
    s <- B.readFile path
    return ((decode s)::(Either String Observations))

makeModel :: Observations -> TrafficModel
makeModel (Observations lengthObs contentObs) =
    let lengthModel = makeLengthModel lengthObs
        contentModel = makeContentModel contentObs
    in TrafficModel lengthModel contentModel

makeLengthModel :: LengthObservations -> PacketLengthModel
makeLengthModel (LengthObservations obs) =
  let (lengths, counts) = unzip obs
      total = sum counts
      probs = map (divideBy total) counts
  in PacketLengthModel $ zip lengths probs

makeContentModel :: ContentObservations -> C.ContentModel
makeContentModel (ContentObservations obs) =
  let (bytes, counts) = unzip obs
      total = sum counts
      probs = map (divideBy total) counts  
  in C.makeContentModel $ zip bytes probs

divideBy :: Integer -> Int -> Double
divideBy d n = 
    let fd = (fromIntegral d)::Double
        fn = (fromIntegral n)::Double
    in fn / fd
