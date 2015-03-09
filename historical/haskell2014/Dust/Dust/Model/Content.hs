{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.Content
(
    ContentModel(..),
    loadContentModel,
    makeContentModel,
    encodeContent,
    decodeContent,
    treeFromProbs
)
where

import Data.Word (Word8)
import System.Random
import GHC.Generics
import Data.Serialize
import Data.Random.Shuffle.Weighted
import Data.Random.RVar
import Data.Random
import Data.Random.Source.IO
import Data.Random.Source.Std
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Dust.Model.Huffman (HuffmanTree)
import Dust.Model.Huffman as H

data ContentModel = ContentModel (HuffmanTree Word8) deriving (Generic)
instance Serialize ContentModel

loadContentModel :: FilePath -> IO ContentModel
loadContentModel path = do
    tree <- H.fileToTree path
    return $ ContentModel $ tree

makeContentModel :: [(Word8, Int)] -> ContentModel
makeContentModel counts =
    let tree = H.countsToTree counts
    in ContentModel $ tree

treeFromProbs :: [Double] -> ContentModel
treeFromProbs probs = do
  let maxint = 2147483647
  let chars = [0..255] :: [Word8]
  let counts = (map fromIntegral $ map round $ map (* maxint) probs) :: [Int]
  let pairs = zip chars counts
  ContentModel $ H.countsToTree pairs

encodeContent :: ContentModel -> B.ByteString -> B.ByteString
encodeContent (ContentModel tree) input =
    let codez = H.codes tree
        bytes = B.unpack input
        bits = H.padToEight $ H.encode codez bytes
    in (B.concat . BL.toChunks) $ H.bitpack bits

decodeContent :: ContentModel -> B.ByteString -> B.ByteString
decodeContent (ContentModel tree) input =
    let bits = H.bitunpack input
        bytes = H.decode tree bits
    in B.pack bytes
