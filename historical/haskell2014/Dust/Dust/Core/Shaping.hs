{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.Shaping
(
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Int
import Data.Binary.Put (Put, putByteString)
import Data.Binary.Get (Get, getByteString)

import Dust.Core.DustPacket
import Dust.Crypto.Cipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Model.TrafficModel
import Dust.Core.CryptoProtocol

{--
readBytes :: TrafficGenerator -> ByteString -> Int -> ByteString -> ByteString
readBytes gen sock maxLen buffer = do
    bs <- recv sock maxLen
    let buff = B.append buffer bs
    let decoder = decodeContent gen
    let decoded = decoder buff
    let decodedLen = B.length decoded
    if decodedLen >= maxLen
      then do
        return $ B.take maxLen decoded
      else do
        result <- readMoreBytes gen sock maxLen buff
        return $ B.take maxLen result

readMoreBytes :: TrafficGenerator -> ByteString -> Int -> ByteString -> ByteString
readMoreBytes gen sock maxLen buffer = do
    bs <- recv sock 1
    let buff = B.append buffer bs
    let decoder = decodeContent gen
    let decoded = decoder buff
    let decodedLen = B.length decoded
    if decodedLen >= maxLen
      then do
        return decoded
      else do
        result <- readMoreBytes gen sock maxLen buff
        return result

putSessionPacket :: TrafficGenerator -> Session -> Plaintext -> ByteString -> [Packet]
putSessionPacket gen session plaintext sock = do
    let (Session (Keypair (PublicKey myPublic) _) _ (IV iv)) = session
--    let encoder = encodeContent gen
--    let encPub = encoder myPublic
--    let encIV = encoder iv
--    let encSession = B.append encPub encIV

    let packet = makePlainPacket plaintext
    let cipher = makeEncrypt session
    let (CipherDataPacket (CipherHeader (Ciphertext header)) (Ciphertext payload)) = encryptData cipher packet
--    let encHeader = encoder header
--    let encPayload = encoder payload
--    let encPacket = B.append encHeader encPayload -- 4 bytes, variable

--    let bytes = B.append encSession encPacket

    let sessionBytes = B.append myPublic iv
    let packetBytes = B.append header payload
    let bytes = B.append sessionBytes packetBytes

    sendBytes gen bytes sock

sendBytes :: TrafficGenerator -> B.ByteString -> [Packet]
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
--}
