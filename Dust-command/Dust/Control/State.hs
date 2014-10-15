{-# LANGUAGE TemplateHaskell #-}

module Dust.Control.State
(
  ControlState,
  newControlState,
  trafficGenerator,
  packetCountRandom,
  lengthRandom,
  portRandom
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Lens.TH (makeLenses)

import Dust.Crypto.PRNG
import Dust.Model.TrafficModel

data ControlState = ControlState {
    _trafficGenerator :: TrafficGenerator,
    _packetCountRandom :: PRNG,
    _lengthRandom :: PRNG,
    _portRandom :: PRNG
  }

makeLenses ''ControlState

newControlState :: TrafficGenerator -> IO ControlState
newControlState gen = do
  r1 <- newPRNG
  r2 <- newPRNG
  r3 <- newPRNG
  return $ ControlState gen r1 r2 r3
