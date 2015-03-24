module Dust.Crypto.ECDH
(
  createKeypair,
  createShared
) where

import Data.ByteString as B
import qualified Crypto.DH.Curve25519 as Curve25519
import qualified Crypto.Key as CK

import Dust.Crypto.Keys
import Dust.Crypto.Cipher
import Dust.Crypto.PRNG

createKeypair :: IO Keypair
createKeypair = do
  ((CK.PublicKey public), (CK.SecretKey private)) <- Curve25519.createKeypair
  return $ Keypair (PublicKey public) (PrivateKey private)

createShared :: PrivateKey -> PublicKey -> EncryptionKey
createShared (PrivateKey private) (PublicKey public) = EncryptionKey $ Curve25519.curve25519 (CK.SecretKey private) (CK.PublicKey public)
