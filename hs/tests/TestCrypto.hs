{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)

import Data.ByteString.Char8 (pack, useAsCString, packCString)

import Dust.Crypto.DustCipher
import Dust.Crypto.Curve25519
import Dust.Crypto.Curve
import Dust.Crypto.Keys

case_keypair_save_load = do
    let input = (pack "1234567890123457890123456789012")
    let keypair1 = createKeypair input
    saveKeypair keypair1

    keypair2 <- loadKeypair

    keypair1 @?= keypair2

case_curve_createShared = do
    let entropy1 = (pack "1234567890123457890123456789012")
    let private1 = createPrivate entropy1
    let public1 = createPublic private1

    let entropy2 = (pack "2109876543210987543210987654321")
    let private2 = createPrivate entropy2
    let public2 = createPublic private2

    let shared1 = createShared private1 public2
    let shared2 = createShared private2 public1

    shared1 @?= shared2

case_curve_createKeypair = do
    let input = (pack "1234567890123457890123456789012")
    let keys = createKeypair input
    let Keypair public private = keys
    publicBytes public @?= pack "\164x}\156n\177\164\240\187F\163\202\232}\221\207/x\ETX\178\SOH\208\238\213q\138%\137\159\253+{"
    privateBytes private @?= pack "023456789012345789012345678901r"

case_curve_createPublic = do
    let input = (pack "1234567890123457890123456789012")
    let private = createPrivate input
    let public = createPublic private
    publicBytes public @?= pack "\164x}\156n\177\164\240\187F\163\202\232}\221\207/x\ETX\178\SOH\208\238\213q\138%\137\159\253+{"

case_curve_createPrivate = do
    let input = (pack "1234567890123457890123456789012")
    let output = createPrivate input
    privateBytes output @?= pack "023456789012345789012345678901r"

case_curve25519_raw_result = do
    useAsCString (pack "1234567890123457890123456789012") $ \output -> do
        useAsCString (pack "1234567890123457890123456789012") $ \input1 -> do
            useAsCString (pack "1234567890123457890123456789012") $ \input2 -> do
                result <- c_curve25519_donna output input1 input2
                result @?= 0

case_curve25519_raw_output = do
    useAsCString (pack "1234567890123457890123456789012") $ \output -> do
        useAsCString (pack "1234567890123457890123456789012") $ \input1 -> do
            useAsCString (pack "1234567890123457890123456789012") $ \input2 -> do
                result <- c_curve25519_donna output input1 input2
                value <- packCString output
                value @?= pack "h\227\189dxK\SO\DC2W\177\128\242v\242\ACK3[a\145&w\190\177G\253*Bg\131\253sr"

case_curve25519_unsafe = do
    let input1 = (pack "1234567890123457890123456789012")
    let input2 = (pack "1234567890123457890123456789012")
    output <- unsafe_curve25519 input1 input2

    output @?= pack "h\227\189dxK\SO\DC2W\177\128\242v\242\ACK3[a\145&w\190\177G\253*Bg\131\253sr"

case_curve25519_safe = let input1 = (pack "1234567890123457890123456789012")
                           input2 = (pack "1234567890123457890123456789012")
                           output = curve25519 input1 input2

                       in output @?= pack "h\227\189dxK\SO\DC2W\177\128\242v\242\ACK3[a\145&w\190\177G\253*Bg\131\253sr"

case_encrypt_decrypt = let key = EncryptionKey (pack "1234567890123456")
                           iv = IV (pack "1234567890123456")

                           plain1 = Plaintext (pack "majestic")
                           cipher = encrypt key iv plain1
                           plain2 = decrypt key iv cipher

                       in plain1 @?= plain2

main :: IO ()
main = defaultMain [$(testGroupGenerator)]
