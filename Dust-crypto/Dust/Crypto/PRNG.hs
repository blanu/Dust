module Dust.Crypto.PRNG
(
  DustPRNG,
  newPRNG,
  makePRNG,
  randomBytes
)
where

import qualified Data.ByteString as B
import System.Entropy
import Dust.Crypto.Hash

data DustPRNG = DustPRNG B.ByteString B.ByteString -- buffer seed

newPRNG :: IO DustPRNG
newPRNG = do
  seed <- getEntropy 32
  let buffer = B.replicate 32 0 `B.append` seed
  return $ makePRNG buffer

makePRNG :: B.ByteString -> DustPRNG
makePRNG seed = do
  let h = digest seed
  let (buffer, seed) = B.splitAt 16 h
  DustPRNG buffer seed

randomBytes :: Int -> DustPRNG -> (B.ByteString, DustPRNG)
randomBytes i prng = do
  if enoughBytes i prng
    then takeBytes i prng
    else randomBytes i (generateBytes prng)

enoughBytes :: Int -> DustPRNG -> Bool
enoughBytes i (DustPRNG buffer seed) = B.length buffer >= i

generateBytes :: DustPRNG -> DustPRNG
generateBytes (DustPRNG buffer seed) = do
  let h = digest seed
  let (bs, seed') = B.splitAt 16 h
  DustPRNG (B.append buffer bs) seed'

takeBytes :: Int -> DustPRNG -> (B.ByteString, DustPRNG)
takeBytes i prng@(DustPRNG buffer seed) =
  if enoughBytes i prng
    then do
      let (result, buffer') = B.splitAt i buffer
      (result, DustPRNG buffer' seed)
    else (B.empty, prng)
