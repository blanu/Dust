{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.WireProtocol
(
 getSession,
 getPacket,
 putSession,
 putPacket
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Int
import Data.Serialize.Put (Put, putByteString)
import Data.Serialize.Get (Get, getByteString)
import Debug.Trace

import Dust.Core.DustPacket
import Dust.Crypto.Cipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Model.TrafficModel
import Dust.Core.CryptoProtocol

getSession :: Keypair -> Get Session
getSession keypair = do
    public <- getByteString 32
    iv     <- getByteString 32
    conf   <- getByteString 32
    return $ Session keypair (PublicKey public) (IV iv) (Confirmation (Ciphertext conf))

putSession :: Session -> Put
putSession (Session (Keypair (PublicKey public) _) _ (IV iv) (Confirmation (Ciphertext conf))) = do
  putByteString public
  putByteString iv
  putByteString conf

getPacket :: Session -> Get Plaintext
getPacket session = do
  packetBytes <- getByteString 4
  let cipherHeader = CipherHeader (Ciphertext packetBytes)
  let cipher = makeCipher session
  let plainPacketHeader = decryptHeader cipher cipherHeader
  let PlainHeader packetLength = plainPacketHeader
  let packetLen = (fromIntegral packetLength)::Int

  payloadBytes <- getByteString packetLen
  let ciphertext = Ciphertext payloadBytes
  let (plaintext, cipher') = decrypt cipher ciphertext
  return plaintext

putPacket :: Session -> Plaintext -> Put
putPacket session plaintext = do
  let packet = makePlainPacket plaintext
  let (CipherDataPacket (CipherHeader (Ciphertext header)) (Ciphertext payload)) = encryptData (makeCipher session) packet
  putByteString header
  putByteString payload
