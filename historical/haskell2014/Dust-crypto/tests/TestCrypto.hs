{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.HUnit hiding (test)
import Test.QuickCheck
import Test.QuickCheck.Gen

import Data.ByteString as B (ByteString, length)
import Data.ByteString.Char8 (pack, useAsCString, packCString)

import Dust.Crypto.Cipher
import qualified Dust.Crypto.ECDH as ECDH
import qualified Dust.Crypto.ECDSA as ECDSA
import Dust.Crypto.Keys

gen_privkeys :: Gen String
gen_privkeys =
  let e = elements ['\00'..'\FF']
  in vectorOf 32 e

prop_ECDH_pubkey_size :: Property
prop_ECDH_pubkey_size =
  forAll gen_privkeys $ \privBytes ->
    let priv = PrivateKey $ pack privBytes
        (PublicKey pub)  = ECDH.createPublic priv
    in B.length pub == 32

prop_ECDSA_pubkey_size :: Property
prop_ECDSA_pubkey_size =
  forAll gen_privkeys $ \privBytes ->
    let priv = PrivateKey $ pack privBytes
        (PublicKey pub)  = ECDSA.createPublic priv
    in B.length pub == 32

case_ECDH_keypair_save_load = do
    let input = (pack "1234567890123457890123456789012")
    let keypair1 = ECDH.createKeypair input
    saveKeypair keypair1

    keypair2 <- loadKeypair

    keypair1 @?= keypair2

case_ECDH_createShared = do
    let entropy1 = (pack "1234567890123457890123456789012")
    let private1 = ECDH.createPrivate entropy1
    let public1 = ECDH.createPublic private1

    let entropy2 = (pack "2109876543210987543210987654321")
    let private2 = ECDH.createPrivate entropy2
    let public2 = ECDH.createPublic private2

    let shared1 = ECDH.createShared private1 public2
    let shared2 = ECDH.createShared private2 public1

    shared1 @?= shared2

case_ECDH_createKeypair = do
    let input = (pack "1234567890123457890123456789012")
    let keys = ECDH.createKeypair input
    let Keypair public private = keys
    publicBytes public @?= pack "\164x}\156n\177\164\240\187F\163\202\232}\221\207/x\ETX\178\SOH\208\238\213q\138%\137\159\253+{"
    privateBytes private @?= pack "023456789012345789012345678901r"

case_ECDH_createPublic = do
    let input = (pack "1234567890123457890123456789012")
    let private = ECDH.createPrivate input
    let public = ECDH.createPublic private
    publicBytes public @?= pack "\164x}\156n\177\164\240\187F\163\202\232}\221\207/x\ETX\178\SOH\208\238\213q\138%\137\159\253+{"

case_ECDH_createPrivate = do
    let input = (pack "1234567890123457890123456789012")
    let output = ECDH.createPrivate input
    privateBytes output @?= pack "023456789012345789012345678901r"

case_encrypt_decrypt = let key = EncryptionKey (pack "1234567890123456")
                           iv = IV (pack "1234567890123456")

                           plain1 = Plaintext (pack "majestic")
                           cipher = encrypt key iv plain1
                           plain2 = decrypt key iv cipher

                       in plain1 @?= plain2

main :: IO ()
main =
 defaultMain [
     testGroup "ECDH25519" [
         testCase "ECDH_createPrivate" case_ECDH_createPrivate,
         testCase "ECDH_createPublic" case_ECDH_createPublic,
         testCase "ECDH_createKeypair" case_ECDH_createKeypair,
         testCase "ECDH_createShared" case_ECDH_createShared,
         testCase "ECDH_keypair_save_load" case_ECDH_keypair_save_load,
         testCase "encrypt_decrypt" case_encrypt_decrypt,

         testProperty "ECDH_pubkey_size" prop_ECDH_pubkey_size,
         testProperty "ECDSA_pubkey_size" prop_ECDSA_pubkey_size
       ]
   ]
