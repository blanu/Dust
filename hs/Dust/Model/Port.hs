{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.Port
(
    PortModel(..)
)
where

import GHC.Generics
import Data.Serialize

data PortModel = PortModel [Int] deriving (Eq, Show, Generic)
instance Serialize PortModel
