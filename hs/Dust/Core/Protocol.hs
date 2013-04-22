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
import System.IO (appendFile)

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

getSession :: TrafficGenerator -> Keypair -> Socket -> IO Session
getSession gen keypair sock = do
    public <- readBytes gen sock 32 B.empty
    iv <- readBytes gen sock 16 B.empty
    return (Session keypair (PublicKey public) (IV iv))

getPacket :: TrafficGenerator -> Session -> Socket -> IO Plaintext
getPacket gen session sock = do
    packetBytes <- readBytes gen sock 4 B.empty
    let cipherHeader = CipherHeader (Ciphertext packetBytes)
    let cipher = makeDecrypt session
    let plainPacketHeader = decryptHeader cipher cipherHeader
    let PlainHeader packetLength = plainPacketHeader
    let packetLen = (fromIntegral packetLength)::Int

    payloadBytes <- readBytes gen sock packetLen B.empty
    let ciphertext = Ciphertext payloadBytes
    return (cipher ciphertext)

readBytes :: TrafficGenerator -> Socket -> Int -> B.ByteString -> IO(B.ByteString)
readBytes gen sock maxLen buffer = do
    bs <- recv sock maxLen
    putStrLn $ "Read: " ++ (show $ B.length bs) ++ "/" ++ (show maxLen)
    let buff = B.append buffer bs
    let decoder = decodeContent gen
    let decoded = decoder buff
    let decodedLen = B.length decoded
    putStrLn $ "Decoded :" ++ (show decodedLen)
    if decodedLen >= maxLen
      then return decoded
      else do
        result <- readMoreBytes gen sock maxLen buff
        return $ B.take maxLen result

readMoreBytes :: TrafficGenerator -> Socket -> Int -> B.ByteString -> IO B.ByteString
readMoreBytes gen sock maxLen buffer = do
    bs <- recv sock 1
    putStrLn $ "Read more: " ++ (show $ B.length bs) ++ "+" ++ (show $ B.length buffer) ++ "/" ++ (show maxLen)
    let buff = B.append buffer bs
    let decoder = decodeContent gen
    let decoded = decoder buff
    let decodedLen = B.length decoded
    if decodedLen >= maxLen
      then return decoded
      else do
        result <- readBytes gen sock maxLen buff
        return $ B.take maxLen result

encodeSession :: TrafficGenerator -> Session -> B.ByteString
encodeSession gen (Session (Keypair (PublicKey myPublic) _) _ (IV iv)) =
    let encoder = encodeContent gen
    in B.append (encoder myPublic) (encoder iv) -- 32 bytes, 16 bytes

encodePacket :: TrafficGenerator -> Session -> Plaintext -> B.ByteString
encodePacket gen session plaintext =
    let packet = makePlainPacket plaintext
        cipher = makeEncrypt session
        (CipherDataPacket (CipherHeader (Ciphertext header)) (Ciphertext payload)) = encryptData cipher packet
        encoder = encodeContent gen
    in B.append (encoder header) (encoder payload) -- 4 bytes, variable

encodeSessionPacket :: TrafficGenerator -> Session -> Plaintext -> B.ByteString
encodeSessionPacket gen session plaintext =
    B.append (encodeSession gen session) (encodePacket gen session plaintext)

putSessionPacket :: TrafficGenerator -> Session -> Plaintext -> Socket -> IO()
putSessionPacket gen session plaintext sock = do
    let bytes = encodeSessionPacket gen session plaintext
    sendBytes gen bytes sock

sendBytes :: TrafficGenerator -> B.ByteString -> Socket -> IO()
sendBytes gen msg sock = do
    let msgLength = B.length msg
    targetPacketLength <- generateLength gen

    putStrLn $ "Lengths: " ++ (show msgLength) ++ " " ++ (show targetPacketLength)

    let bs = case compare targetPacketLength msgLength of
                GT -> pad msg $ fromIntegral (targetPacketLength - msgLength)
                otherwise -> msg

    let (part, rest) = B.splitAt (fromIntegral targetPacketLength) bs

    putStrLn $ "Sending with target packet length of " ++ (show $ B.length part)
    appendFile "targetLenths.txt" $ show targetPacketLength ++ "\n"
    sendAll sock part

    if not $ B.null rest
        then sendBytes gen rest sock
        else return ()

pad :: B.ByteString -> Int -> B.ByteString
pad bs amount = B.append bs $ B.replicate amount 0
