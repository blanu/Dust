module Dust.Crypto.PRNG
(
  PRNG,
  newPRNG,
  makePRNG,
  randomBytes,
  randomWord16,
  randomDouble
)
where

import qualified Data.ByteString as B
import System.Entropy
import Dust.Crypto.Hash
import Control.Monad.State.Lazy
import Data.Word
import Data.Serialize (decode)
import Data.Int
import System.Random

data PRNG = PRNG B.ByteString B.ByteString -- buffer seed

instance RandomGen PRNG where
  next prng = do
    let (result, prng') = randomWord16 prng
    (fromIntegral result, prng')
  genRange prng = (0, 65535)
  split prng = do
    let (result, prng') = randomWord16 prng
    (prng, prng')

newPRNG :: IO PRNG
newPRNG = do
  seed <- getEntropy 32
  let buffer = B.replicate 32 0 `B.append` seed
  return $ makePRNG buffer

makePRNG :: B.ByteString -> PRNG
makePRNG seed = do
  let h = digest seed
  let (buffer, seed) = B.splitAt 16 h
  PRNG buffer seed

randomBytes :: Int -> PRNG -> (B.ByteString, PRNG)
randomBytes i prng = do
  if enoughBytes i prng
    then takeBytes i prng
    else randomBytes i (generateBytes prng)

randomBytes' :: Int -> State PRNG B.ByteString
randomBytes' i = do
  prng <- get
  let (result, prng') = randomBytes i prng
  put prng'
  return result

randomWord16 :: PRNG -> (Word16, PRNG)
randomWord16 prng = do
  let (bs, prng') = randomBytes 2 prng
  let maybeResult = (decode bs) :: Either String Word16
  case maybeResult of
    Left _ -> (0, prng')
    Right result -> (result, prng')

randomWord16' :: Int -> State PRNG Word16
randomWord16' i = do
  prng <- get
  let (result, prng') = randomWord16 prng
  put prng'
  return result

randomDouble :: PRNG -> (Double, PRNG)
randomDouble prng = do
  let intMaxDouble = 4503599627370495 :: Word64
  let maxDouble = (fromIntegral intMaxDouble) :: Double
  let (bs, prng') = randomBytes 8 prng
  let maybeResult = (decode bs) :: Either String Word64
  case maybeResult of
    Left _ -> (0, prng')
    Right i -> do
      let m = i `mod` intMaxDouble -- wrap to 52 bits
      let d = (fromIntegral m) :: Double -- convert to double with 0 exponent
      let result = d/maxDouble -- scale to [0,1] range
      (result, prng') -- scale by max Double constant

enoughBytes :: Int -> PRNG -> Bool
enoughBytes i (PRNG buffer seed) = B.length buffer >= i

generateBytes :: PRNG -> PRNG
generateBytes (PRNG buffer seed) = do
  let h = digest seed
  let (bs, seed') = B.splitAt 16 h
  PRNG (B.append buffer bs) seed'

takeBytes :: Int -> PRNG -> (B.ByteString, PRNG)
takeBytes i prng@(PRNG buffer seed) =
  if enoughBytes i prng
    then do
      let (result, buffer') = B.splitAt i buffer
      (result, PRNG buffer' seed)
    else (B.empty, prng)
