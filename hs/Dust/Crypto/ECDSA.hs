{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.ECDSA
(
  Signature,
  createPrivate,
  createPublic,
  createKeypair,
  sign,
  verify
) where

import Data.ByteString as B
import Data.Word
import Data.Bits
import System.Entropy
import GHC.Generics
import Data.Serialize

import Dust.Crypto.Keys
import Dust.Crypto.Ed25519

newtype Signature = Signature { signatureBytes :: ByteString } deriving (Show, Eq, Generic)
instance Serialize Signature

createPrivate :: ByteString -> PrivateKey
createPrivate bs = PrivateKey bs

createPublic :: PrivateKey -> PublicKey
createPublic private = PublicKey $ ed25519_publickey $ privateBytes private

createKeypair :: ByteString -> Keypair
createKeypair entropy = let private = createPrivate entropy
                            public = createPublic private
                        in Keypair public private

sign :: ByteString -> PrivateKey -> PublicKey -> Signature
sign msg private pubkey = Signature $ ed25519_sign msg (privateBytes private) (publicBytes pubkey)

verify :: ByteString -> PublicKey -> Signature -> Bool
verify msg pubkey signature = ed25519_sign_open msg (publicBytes pubkey) (signatureBytes signature)
