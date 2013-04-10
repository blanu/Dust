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

data Signedtext = Signedtext PublicKey Signature ByteString deriving (Eq, Show, Generic)
instance Serialize Signedtext

createPrivate :: ByteString -> PrivateKey
createPrivate bs = PrivateKey bs

createPublic :: PrivateKey -> PublicKey
createPublic private = PublicKey $ ed25519_publickey $ privateBytes private

createSigningKeypair :: ByteString -> Keypair
createSigningKeypair entropy =
    let private = createPrivate entropy
        public = createPublic private
    in Keypair public private

sign :: ByteString -> Keypair -> Signedtext
sign msg (Keypair pubkey private) = Signedtext pubkey (Signature $ ed25519_sign msg (privateBytes private) (publicBytes pubkey)) msg

verify :: Signedtext -> Bool
verify (Signedtext pubkey signature msg) = ed25519_sign_open msg (publicBytes pubkey) (signatureBytes signature)
