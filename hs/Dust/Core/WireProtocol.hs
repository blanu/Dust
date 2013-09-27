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
import Data.Binary.Put (Put, putByteString)
import Data.Binary.Get (Get, getByteString)

import Dust.Core.DustPacket
import Dust.Crypto.DustCipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Model.TrafficModel
import Dust.Core.CryptoProtocol

getSession :: Keypair -> Get Session
getSession keypair = do
    public <- getByteString 32
    iv     <- getByteString 16
    return $ Session keypair (PublicKey public) (IV iv)

putSession :: Session -> Put
putSession (Session (Keypair (PublicKey public) _) _ (IV iv)) = do
  putByteString public
  putByteString iv

getPacket :: Session -> Get Plaintext
getPacket session = do
  packetBytes <- getByteString 4
  let cipherHeader = CipherHeader (Ciphertext packetBytes)
  let cipher = makeDecrypt session
  let plainPacketHeader = decryptHeader cipher cipherHeader
  let PlainHeader packetLength = plainPacketHeader
  let packetLen = (fromIntegral packetLength)::Int

  payloadBytes <- getByteString packetLen
  let ciphertext = Ciphertext payloadBytes
  return $ cipher ciphertext

putPacket :: Session -> Plaintext -> Put
putPacket session plaintext = do
  let packet = makePlainPacket plaintext
  let (CipherDataPacket (CipherHeader (Ciphertext header)) (Ciphertext payload)) = encryptData (makeEncrypt session) packet
  putByteString header
  putByteString payload
