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
 makeEncoder,
 getSession,
 getPacket,
 putSession,
 putPacket
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Int
import Data.ByteString.Lazy (ByteString, append, toChunks)
import Data.Binary.Put (Put, putByteString)
import Data.Binary.Get (Get, getLazyByteString, runGet, runGetState)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

import Dust.Core.DustPacket
import Dust.Crypto.DustCipher
import Dust.Crypto.Curve
import Dust.Crypto.Keys

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

getSession :: Keypair -> Socket -> IO Session
getSession keypair sock = do
    public <- recv sock 32
    iv <- recv sock 16
    return (Session keypair (PublicKey public) (IV iv))

getPacket :: Session -> Socket -> IO Plaintext
getPacket session sock = do
    packetBytes <- recv sock 4
    let cipherHeader = CipherHeader (Ciphertext packetBytes)
    let cipher = makeDecrypt session
    let plainPacketHeader = decryptHeader cipher cipherHeader
    let PlainHeader packetLength = plainPacketHeader

--    payloadBytes <- recv sock ((fromIntegral packetLength)::Int64)
    payloadBytes <- recv sock ((fromIntegral packetLength)::Int)
    let ciphertext = Ciphertext payloadBytes
    return (cipher ciphertext)

putSession :: Session -> Socket -> IO()
putSession (Session (Keypair (PublicKey myPublic) _) _ (IV iv)) sock = do
    sendAll sock myPublic
    sendAll sock iv
    return ()

putPacket :: Session -> Plaintext -> Socket -> IO()
putPacket session plaintext sock = do
    let packet = makePlainPacket plaintext
    let cipher = makeEncrypt session
    let (CipherDataPacket (CipherHeader (Ciphertext header)) (Ciphertext payload)) = encryptData cipher packet
    sendAll sock header
    sendAll sock payload
    return()
