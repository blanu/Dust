{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.Cipher
(
  EncryptionKey(..),
  IV(..),
  Plaintext(..),
  Ciphertext(..),
  Cipher,

  newCipherWithKey,
  newCipherIV,
  encrypt,
  decrypt,
  createIV
) where

import GHC.Generics
import Data.Serialize
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Entropy
import Data.Bits
import Data.Word

import Dust.Crypto.Keys
import Dust.Crypto.PRNG

data EncryptionKey = EncryptionKey B.ByteString deriving (Show, Eq)
newtype IV = IV B.ByteString deriving (Show, Eq, Generic)
newtype Plaintext = Plaintext B.ByteString deriving (Show, Eq, Generic)
newtype Ciphertext = Ciphertext B.ByteString deriving (Show, Eq, Generic)

instance Serialize IV
instance Serialize Plaintext
instance Serialize Ciphertext

data Cipher = Cipher PRNG

newCipherWithKey :: EncryptionKey -> Cipher
newCipherWithKey (EncryptionKey keyBytes) = do
  let prng = makePRNG keyBytes
  Cipher prng

newCipherIV :: EncryptionKey -> IV -> Cipher
newCipherIV (EncryptionKey keyBytes) (IV ivBytes) = do
  let prng = makePRNG (B.append keyBytes ivBytes)
  Cipher prng

createIV :: PRNG -> (IV, PRNG)
createIV rand =
    let (ivBytes, rand') = randomBytes 32 rand
    in (IV ivBytes, rand')

encrypt :: Cipher -> Plaintext -> (Ciphertext, Cipher)
encrypt cipher (Plaintext plaintext) = do
  let (ciphertext, cipher') = transform cipher plaintext
  (Ciphertext ciphertext, cipher')

decrypt :: Cipher -> Ciphertext -> (Plaintext, Cipher)
decrypt cipher (Ciphertext ciphertext) = do
  let (plaintext, cipher') = transform cipher ciphertext
  (Plaintext plaintext, cipher')

transform :: Cipher -> B.ByteString -> (B.ByteString, Cipher)
transform (Cipher prng) bs = do
  let len = B.length bs
  let (entropy, prng') = randomBytes len prng
  let pairs = zip (B.unpack bs) (B.unpack entropy)
  let rb = map xorPair pairs
  (B.pack rb, Cipher prng')

xorPair :: (Word8, Word8) -> Word8
xorPair (a, b) = xor a b
