{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.DustCipher
(
  EncryptionKey(..),
  IV(..),
  Plaintext(..),
  Ciphertext(..),

  encrypt,
  decrypt,
  createIV
) where

import GHC.Generics
import Data.ByteString
import Data.Serialize
import System.Entropy
import Data.ByteString.Lazy (toChunks, fromChunks, toStrict)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Crypto.Threefish
import qualified Crypto.Threefish.Skein.StreamCipher as SSC

import Dust.Crypto.Keys

data EncryptionKey = EncryptionKey ByteString deriving (Show, Eq)
newtype IV = IV ByteString deriving (Show, Eq, Generic)
newtype Plaintext = Plaintext ByteString deriving (Show, Eq, Generic)
newtype Ciphertext = Ciphertext ByteString deriving (Show, Eq, Generic)

instance Serialize IV
instance Serialize Plaintext
instance Serialize Ciphertext

encrypt :: EncryptionKey -> IV -> Plaintext -> Ciphertext
encrypt (EncryptionKey keyBytes) (IV ivBytes) (Plaintext plaintext) =
  let lazy = fromChunks [plaintext]
      maybeKey = toBlock keyBytes
      maybeIv = toBlock ivBytes
  in case (maybeKey, maybeIv) of
    (Just key, Just iv) -> Ciphertext $ toStrict $ SSC.encrypt key iv lazy
    otherwise           -> Ciphertext B.empty

decrypt :: EncryptionKey -> IV -> Ciphertext -> Plaintext
decrypt (EncryptionKey keyBytes) (IV ivBytes) (Ciphertext ciphertext) =
  let lazy = fromChunks [ciphertext]
      maybeKey = toBlock keyBytes
      maybeIv = toBlock ivBytes
  in case (maybeKey, maybeIv) of
    (Just key, Just iv) -> Plaintext $ toStrict $ SSC.decrypt key iv lazy
    otherwise           -> Plaintext B.empty

createIV :: IO (IV)
createIV = do
    entropy <- getEntropy 32
    return (IV entropy)
