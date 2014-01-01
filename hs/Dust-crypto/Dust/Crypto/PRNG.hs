module Dust.Crypto.PRNG
(
  DustPRNG(..),
  newPRNG,
  randomBytes
)
where

import Data.ByteString

import Crypto.Threefish.Random hiding (randomBytes)
import qualified Crypto.Threefish.Random as TR

newtype DustPRNG = DustPRNG SkeinGen

newPRNG :: IO DustPRNG
newPRNG = do
  gen <- newSkeinGen
  return $ DustPRNG gen

randomBytes :: Int -> DustPRNG -> (ByteString, DustPRNG)
randomBytes i (DustPRNG gen) = 
  let (bytes, gen') = TR.randomBytes i gen
  in (bytes, DustPRNG gen')
