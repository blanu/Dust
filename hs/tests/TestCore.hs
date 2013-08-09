{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import qualified Data.ByteString.Lazy as LZ

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework

import Dust.Model.Huffman (bitpack, bitunpack, padToEight)

-- | Bits of the sort that can be bitpack'd and then bitunpack'd
newtype Bits = Bits [Bool] deriving (Show)

instance Arbitrary Bits where
    arbitrary = fmap (Bits . padToEight) $ arbitrary `suchThat` startsWithTrue
        where
        startsWithTrue (True:_) = True
        startsWithTrue _ = False

prop_bitpack_unpack_loop :: Bits -> Bool
prop_bitpack_unpack_loop (Bits bits) = bitunpack packed == bits
    where
    packed = mconcat $ LZ.toChunks $ bitpack bits
    {-# NOINLINE packed #-}

main :: IO ()
main = defaultMain [
     testGroup "Huffman" [
         testProperty "bit pack/unpack loop" prop_bitpack_unpack_loop
       ]
   ]
