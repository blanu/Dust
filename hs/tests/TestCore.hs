{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)

import Data.ByteString.Char8 (pack)

import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Core.Protocol
import Dust.Crypto.Keys
import Dust.Crypto.Curve

case_encode_decode = do
    let iv = IV (pack "1234567890123456")
    let entropy1 = pack "01234567890123456789012345678901"
    let entropy2 = pack "01234567890123456789012345678902"

    let keypair1 = createKeypair entropy1
    let keypair2 = createKeypair entropy2
    let Keypair public _ = keypair2

    let payload1 = (pack "majestic")
    let ser = encode keypair1 public iv payload1
    let payload2 = decode keypair2 ser
    payload1 @=? payload2

case_encode_serialize_conversation = do
    let iv = IV (pack "1234567890123456")
    let entropy1 = pack "01234567890123456789012345678901"
    let entropy2 = pack "01234567890123456789012345678902"

    let keypair1 = createKeypair entropy1
    let keypair2 = createKeypair entropy2
    let Keypair public _ = keypair2

    let payload1 = (pack "majestic")
    let conv1 = encodeConversation keypair1 public iv payload1
    let ser = serializeConversation conv1
    let conv2 = deserializeConversation ser
    let payload2 = decodeConversation keypair2 conv2
    payload1 @=? payload2

case_encode_conversation = do
    let iv = IV (pack "1234567890123456")
    let entropy1 = pack "01234567890123456789012345678901"
    let entropy2 = pack "01234567890123456789012345678902"

    let keypair1 = createKeypair entropy1
    let keypair2 = createKeypair entropy2
    let Keypair public _ = keypair2

    let payload1 = (pack "majestic")
    let conv = encodeConversation keypair1 public iv payload1
    let payload2 = decodeConversation keypair2 conv
    payload1 @=? payload2

case_encode_serialize_packet = do
    let iv = IV (pack "1234567890123456")
    let key = EncryptionKey (pack "1234567890123456")

    let payload1 = (pack "majestic")
    let cipher1 = encodePacket iv key payload1
    let ser = serializePacket cipher1
    let cipher2 = deserializePacket ser
    let payload2 = decodePacket key cipher2
    payload1 @?= payload2

case_encode_packet = do
    let iv = IV (pack "1234567890123456")
    let key = EncryptionKey (pack "1234567890123456")

    let payload1 = (pack "majestic")
    let cipher = encodePacket iv key payload1
    let payload2 = decodePacket key cipher
    payload1 @?= payload2

case_serialize_packet = do
    let key = EncryptionKey (pack "1234567890123456")
    let iv = IV (pack "1234567890123456")
    let payload = (pack "majestic")

    let plain1 = PlainDataPacket (DustPacketHeader iv) (Plaintext payload)
    let cipher1 = encryptData key plain1
    let cipher2 = deserializePacket (serializePacket cipher1)
    cipher1 @?= cipher2

case_packets = do
    let key = EncryptionKey (pack "1234567890123456")
    let iv = IV (pack "1234567890123456")
    let payload = (pack "majestic")

    let plain1 = PlainDataPacket (DustPacketHeader iv) (Plaintext payload)
    let cipher = encryptData key plain1
    let plain2 = decryptData key cipher
    plain1 @?= plain2

main :: IO ()
main = defaultMain [$(testGroupGenerator)]
