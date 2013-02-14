{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)

import Dust.Crypto.DustCipher
import Data.ByteString.Char8 (pack)

case_packets = let key = Key (pack "1234567890123456")
                   iv = IV (pack "1234567890123456")

                   plain1 = Plaintext (pack "majestic")
                   cipher = encrypt key iv plain1
                   plain2 = decrypt key iv cipher

               in plain1 @?= plain2

main :: IO ()
main = defaultMain [$(testGroupGenerator)]
