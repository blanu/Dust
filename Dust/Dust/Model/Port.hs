{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.Port
(
    PortModel(..),
    nextPort
)
where

import GHC.Generics
import Data.Serialize
import Data.Random
import Data.Random.RVar
import Data.Random.Extras
import Data.Random.Source.IO
import Data.Random.Source.Std

data PortModel = PortModel [Int] deriving (Eq, Show, Generic)
instance Serialize PortModel

nextPort :: [Int] -> IO Int
nextPort ports =
  let dist = choice ports
  in runRVar dist StdRandom :: IO Int

