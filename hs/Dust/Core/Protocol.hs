{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.Protocol
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

import GHC.Generics
import qualified Data.ByteString as B
import Data.ByteString (ByteString, append)
import Data.Serialize

import Dust.Core.DustPacket
import Dust.Crypto.DustCipher
import Dust.Crypto.Curve
import Dust.Crypto.Keys

data Session = Session Keypair PublicKey IV deriving (Show)
data Stream = Stream StreamHeader CipherDataPacket deriving (Show, Generic)
data StreamHeader = StreamHeader PublicKey IV deriving (Show, Generic)

instance Serialize Stream
instance Serialize StreamHeader

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
makeEncoder session@(Session keypair otherPublic iv) =
    let header = makeHeader otherPublic iv
        cipher = makeEncrypt session
        encrypter = encryptData cipher
        stream = makeStream header
    in stream . encrypter . makePlainPacket
