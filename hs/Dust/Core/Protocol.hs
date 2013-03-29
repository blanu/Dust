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
 putSessionPacket
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
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Model.PacketLength

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
    let packetLen = (fromIntegral packetLength)::Int

    payloadBytes <- readBytes sock packetLen
    let ciphertext = Ciphertext payloadBytes
    return (cipher ciphertext)

readBytes :: Socket -> Int -> IO(B.ByteString)
readBytes sock maxLen = do
    bs <- recv sock maxLen
    let readLen = B.length bs
    if readLen == maxLen
      then return bs
      else do
        rest <- readBytes sock (maxLen-readLen)
        return $ B.append bs rest

encodeSession :: Session -> B.ByteString
encodeSession (Session (Keypair (PublicKey myPublic) _) _ (IV iv)) = B.append myPublic iv

encodePacket :: Session -> Plaintext -> B.ByteString
encodePacket session plaintext =
    let packet = makePlainPacket plaintext
        cipher = makeEncrypt session
        (CipherDataPacket (CipherHeader (Ciphertext header)) (Ciphertext payload)) = encryptData cipher packet
    in B.append header payload

encodeSessionPacket :: Session -> Plaintext -> B.ByteString
encodeSessionPacket session plaintext = B.append (encodeSession session) (encodePacket session plaintext)

putSessionPacket :: Session -> Plaintext -> Socket -> IO()
putSessionPacket session plaintext sock = do
    let bytes = encodeSessionPacket session plaintext
    sendBytes bytes sock

sendBytes :: B.ByteString -> Socket -> IO()
sendBytes msg sock = do
    let msgLength = toInteger $ B.length msg
    targetPacketLength <- nextLength

    let bs = case compare targetPacketLength msgLength of
                GT -> pad msg $ fromIntegral (targetPacketLength - msgLength)
                otherwise -> msg

    let (part, rest) = B.splitAt (fromIntegral targetPacketLength) msg

    sendAll sock part

    if not $ B.null rest
        then sendBytes rest sock
        else return ()

pad :: B.ByteString -> Int -> B.ByteString
pad bs amount = B.append bs $ B.replicate amount 0
