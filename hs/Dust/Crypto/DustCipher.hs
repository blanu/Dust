module Dust.Crypto.DustCipher
(
  Key(..),
  IV(..),
  Plaintext(..),
  Ciphertext(..),

  encrypt,
  decrypt

) where

import Data.ByteString
import Codec.Crypto.AES

newtype Key = Key { keyBytes :: ByteString } deriving (Show, Eq)
newtype IV = IV { ivBytes :: ByteString } deriving (Show, Eq)
newtype Plaintext = Plaintext { plainBytes :: ByteString } deriving (Show, Eq)
newtype Ciphertext = Ciphertext { cipherBytes :: ByteString } deriving (Show, Eq)

encrypt :: Key -> IV -> Plaintext -> Ciphertext
encrypt key iv plaintext = Ciphertext (crypt' CTR (keyBytes key) (ivBytes iv) Encrypt (plainBytes plaintext))

decrypt :: Key -> IV -> Ciphertext -> Plaintext
decrypt key iv ciphertext = Plaintext (crypt' CTR (keyBytes key) (ivBytes iv) Decrypt (cipherBytes ciphertext))
