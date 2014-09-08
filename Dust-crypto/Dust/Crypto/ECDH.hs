module Dust.Crypto.ECDH
(
  createKeypair,
  createShared
) where

import Data.ByteString as B
import Data.Word
import Data.Bits
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.DH.Curve25519 as Curve25519

import Dust.Crypto.Keys
import Dust.Crypto.DustCipher
import Dust.Crypto.PRNG

createKeypair :: IO Keypair
createKeypair = do
  ((Curve25519.PublicKey public), (Curve25519.SecretKey private)) <- Curve25519.createKeypair
  return $ Keypair (PublicKey public) (PrivateKey private)

createShared :: PrivateKey -> PublicKey -> EncryptionKey
createShared (PrivateKey private) (PublicKey public) = EncryptionKey $ Curve25519.curve25519 (Curve25519.SecretKey private) (Curve25519.PublicKey public)
