{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.CryptoProtocol
(
 Session(..),
 Confirmation(..),
 Stream(..),
 StreamHeader(..),
 makeSession,
 makeCipher,
 makeHeader,
 makeStream,
 makeEncoder,
 confirmSession
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Debug.Trace

import Dust.Core.DustPacket
import Dust.Crypto.Cipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Crypto.Hash
import Dust.Crypto.Cipher
import Dust.Model.TrafficModel hiding (Stream)

data Session = Session Keypair PublicKey IV Confirmation deriving (Show, Eq)
data Confirmation = Confirmation Ciphertext deriving (Show, Eq)
data Stream = Stream StreamHeader CipherDataPacket deriving (Show)
data StreamHeader = StreamHeader PublicKey IV deriving (Show)

makeSession :: Keypair -> PublicKey -> IV -> Session
makeSession keypair publicKey iv =
  let (conf, enc') = makeConfirmation keypair publicKey iv
  in Session keypair publicKey iv conf

confirmSession :: Session -> Bool
confirmSession session@(Session keypair publicKey iv _) =
    let session' = makeSession keypair publicKey iv
    in session == session'

makeConfirmation :: Keypair -> PublicKey -> IV -> (Confirmation, Cipher)
makeConfirmation keypair@(Keypair myPublic@(PublicKey pubbs) myPrivate) otherPublic@(PublicKey otherbs) iv@(IV ivbs) =
    let (pubA, pubB) = sortBytes pubbs otherbs
        plainConf = (pubA `B.append` pubB) `B.append` ivbs
        h = digest plainConf
        temp = Session keypair otherPublic iv (Confirmation $ Ciphertext B.empty)
        cipher = makeCipher temp
        (conf, cipher') = encrypt cipher $ Plaintext h
    in (Confirmation conf, cipher')

sortBytes :: ByteString -> ByteString -> (ByteString, ByteString)
sortBytes a b =
    if a < b
        then (a, b)
        else (b, a)

makeCipher :: Session -> Cipher
makeCipher (Session keypair@(Keypair myPublic myPrivate) otherPublic iv _) =
    let key = createShared myPrivate otherPublic
    in  newCipher key iv

makeHeader :: PublicKey -> IV -> StreamHeader
makeHeader publicKey iv = StreamHeader publicKey iv

makeStream :: StreamHeader -> CipherDataPacket -> Stream
makeStream header cipherPacket = Stream header cipherPacket

makeEncoder :: Session -> (Plaintext -> Stream)
makeEncoder session@(Session (Keypair myPublic _) _ iv _) =
    let header = makeHeader myPublic iv
        cipher = makeCipher session
        encrypter = encryptData cipher
        stream = makeStream header
    in stream . encrypter . makePlainPacket
