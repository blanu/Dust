{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.CryptoProtocol
(
 Session(..),
 Stream(..),
 StreamHeader(..),
 makeSession,
 makeEncrypt,
 makeDecrypt,
 makeHeader,
 makeStream,
 makeEncoder
) where

import Dust.Core.DustPacket
import Dust.Crypto.DustCipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Model.TrafficModel

data Session = Session Keypair PublicKey IV deriving (Show)
data Stream = Stream StreamHeader CipherDataPacket deriving (Show)
data StreamHeader = StreamHeader PublicKey IV deriving (Show)

makeSession :: Keypair -> PublicKey -> IV -> Session
makeSession keypair publicKey iv = Session keypair publicKey iv

makeEncrypt :: Session -> (Plaintext -> Ciphertext)
makeEncrypt (Session (Keypair myPublic myPrivate) otherPublic iv) =
    let key = createShared myPrivate otherPublic
    in encrypt key iv

makeDecrypt :: Session -> (Ciphertext -> Plaintext)
makeDecrypt (Session (Keypair myPublic myPrivate) otherPublic iv) =
    let key = createShared myPrivate otherPublic
    in decrypt key iv

makeHeader :: PublicKey -> IV -> StreamHeader
makeHeader publicKey iv = StreamHeader publicKey iv

makeStream :: StreamHeader -> CipherDataPacket -> Stream
makeStream header cipherPacket = Stream header cipherPacket

makeEncoder :: Session -> (Plaintext -> Stream)
makeEncoder session@(Session (Keypair myPublic _) _ iv) =
    let header = makeHeader myPublic iv
        cipher = makeEncrypt session
        encrypter = encryptData cipher
        stream = makeStream header
    in stream . encrypter . makePlainPacket
