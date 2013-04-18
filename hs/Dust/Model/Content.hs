{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.Content
(
    ContentModel(..),
    loadContentModel,
    encodeContent,
    decodeContent
)
where

import Data.Word (Word8)
import System.Random
import GHC.Generics
import Data.Serialize
import Data.Random.Shuffle.Weighted
import Data.Random.RVar
import Data.Random
import Data.Random.Source.DevRandom
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

encodeContent :: (HuffmanTree Word8) -> B.ByteString -> B.ByteString
encodeContent tree input =
    let codez = H.codes tree
        bytes = B.unpack input
        bits = H.padToEight $ H.encode codez bytes
    in (B.concat . BL.toChunks) $ H.bitpack bits

decodeContent :: (HuffmanTree Word8) -> B.ByteString -> B.ByteString
decodeContent tree input =
    let Right bits = H.bitunpack input
        bytes = H.decode tree bits
    in B.pack bytes
