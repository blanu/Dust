{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)

import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Data.ByteString.Char8 (pack)

case_packets = let key = Key (pack "1234567890123456")
                   iv = IV (pack "1234567890123456")
                   payload = (pack "majestic")

                   plain1 = PlainDataPacket (DustPacketHeader iv) (Plaintext payload)
                   cipher = encryptData key plain1
                   plain2 = decryptData key cipher
               in plain1 @?= plain2

main :: IO ()
main = defaultMain [$(testGroupGenerator)]
