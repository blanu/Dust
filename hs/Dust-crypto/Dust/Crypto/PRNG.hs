module Dust.Crypto.PRNG
(
  DustGen,
  newDustGen,
  randomBytes
)
where

import Data.ByteString (ByteString)
import qualified Crypto.Threefish.Random as CTR

data DustGen = DustGen CTR.SkeinGen

newDustGen :: IO DustGen
newDustGen = do
  gen <- CTR.newSkeinGen
  return $ DustGen gen

randomBytes :: Int -> DustGen -> (ByteString, DustGen)
randomBytes n (DustGen gen) =
  let (bytes, gen') = CTR.randomBytes n gen
  in (bytes, DustGen gen')
