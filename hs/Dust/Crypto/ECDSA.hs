{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.ECDSA
(
  Signature,
  Signedtext(..),
  createPrivate,
  createPublic,
  createSigningKeypair,
  sign,
  verify
) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.ByteString (ByteString)

import Dust.Crypto.Keys
import Dust.Crypto.Ed25519

newtype Signature = Signature { signatureBytes :: ByteString } deriving (Show, Eq, Generic)
instance Serialize Signature

data Signedtext = Signedtext PublicKey Signature ByteString deriving (Eq, Show, Generic)
instance Serialize Signedtext

createPrivate :: ByteString -> PrivateKey
createPrivate = PrivateKey

createPublic :: PrivateKey -> PublicKey
createPublic = PublicKey . ed25519_publickey . privateBytes

createSigningKeypair :: ByteString -> Keypair
createSigningKeypair entropy = Keypair (createPublic private) private
    where
    private = createPrivate entropy

sign :: ByteString -> Keypair -> Signedtext
sign msg (Keypair pubkey private) = Signedtext pubkey (Signature $ ed25519_sign msg (privateBytes private) (publicBytes pubkey)) msg

verify :: Signedtext -> Bool
verify (Signedtext pubkey signature msg) = ed25519_sign_open msg (publicBytes pubkey) (signatureBytes signature)
