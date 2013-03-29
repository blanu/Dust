module Dust.Model.PacketLength
(
    nextLength
)
where

import System.Random

nextLength :: IO Integer
nextLength = do
    randomRIO (1, 1500)
