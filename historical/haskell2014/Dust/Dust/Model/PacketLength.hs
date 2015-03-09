{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.PacketLength
(
    PacketLengthModel(..),
    nextLength
)
where

import GHC.Generics (Generic)
import qualified Data.ByteString as B
import Data.Serialize (Serialize, decode)
import Control.Monad.State.Lazy (State(..), get, put)
import Data.Word (Word16)
import Debug.Trace

import Dust.Crypto.PRNG (PRNG, randomDouble)

data PacketLengthModel = PacketLengthModel [Double] deriving (Eq, Show, Generic)

instance Serialize PacketLengthModel

nextLength :: [Double] -> State PRNG Word16
nextLength weights = do
    prng <- get
    let (r, prng') = randomDouble prng
    traceShow r $ put prng'
    let result = weightedSample weights r
    return result
    -- let dist = weightedSampleCDF 1 cdf
    -- arr <- runRVar dist StdRandom :: IO [Int]
    -- return (head arr)

weightedSample :: [Double] -> Double -> Word16
weightedSample weights r = traceShow weights $ weightedSampleWithIndex 0 weights r

weightedSampleWithIndex :: Word16 -> [Double] -> Double -> Word16
weightedSampleWithIndex index [] r = index
weightedSampleWithIndex index (weight:weights) r = do
  let r' = traceShow (r,weight,r-weight) $ r - weight
  if r < 0
    then traceShow (r,index) $ index
    else traceShow (r,index) $ weightedSampleWithIndex (index+1) weights r'
