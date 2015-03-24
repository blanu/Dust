{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.ECDSA
(
  Signature,
  createSigningKeypair,
  sign,
  verify
) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Dust.Crypto.Keys
import qualified Crypto.Key as CK
import qualified Crypto.Sign.Ed25519 as Ed

newtype Signature = Signature { signatureBytes :: ByteString } deriving (Show, Eq, Generic)
instance Serialize Signature

-- FIXME - uses system entropy, but it would be better to pass in the PRNG
createSigningKeypair :: IO Keypair
createSigningKeypair = do
    (CK.PublicKey pub, CK.SecretKey priv) <- Ed.createKeypair
    return $ Keypair (PublicKey pub) (PrivateKey priv)

sign :: ByteString -> PrivateKey -> Signature
sign bs (PrivateKey private) =
    let (Ed.Signature sig) = Ed.sign' (CK.SecretKey private) bs
    in Signature sig

verify :: ByteString -> Signature -> PublicKey -> Bool
verify bs (Signature sig) (PublicKey public) = Ed.verify' (CK.PublicKey public) bs (Ed.Signature sig)
