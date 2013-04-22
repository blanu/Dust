{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)

import Dust.Model.Content

case_encode_decode = do
    (ContentModel tree) <- loadContentModel "/usr/share/dict/american-english"
    let msg1 = pack "qwerty"
    let len = B.length msg1
    putStrLn (show msg1)
    let enc = encodeContent tree msg1
    putStrLn (show enc)
    let msg2 = B.take len $ decodeContent tree enc
    putStrLn (show msg2)

    msg1 @=? msg2

main :: IO ()
main = defaultMain [$(testGroupGenerator)]
