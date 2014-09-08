module Dust.Model.Stats
(
  histogram
)
where

import qualified Data.Map as M
import Data.List (foldl')

-- count the number of instances each symbol occurs in a list
histogram :: Ord a => [a] -> [(a,Int)]
histogram xs =
    M.toList . foldl' insert M.empty $ xs
  where
    insert a k = M.insertWith' (+) k 1 a

