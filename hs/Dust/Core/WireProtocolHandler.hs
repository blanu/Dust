{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.WireProtocol
(
 Packets(..),
 encodeMessage,
 decodeMessage,
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

data Packets = Packets [ByteString]

encodeMessage :: Session -> Plaintext -> Packets
encodeMessage session plaintext = do
  let header = runPut (putSession session)
  let packet = runPut (putPacket session plaintext)
  Packets [B.append header packet]

decodeMessage :: Keypair -> ByteString -> Either String (Plaintext, ByteString)
decodePackets keypair buffer = do
  let eitherSession = decodeSession keypair buffer
  case eitherSession of
    Left error -> eitherSession
    Right (session, rest) -> decodeMessage session rest

decodeSession :: Keypair -> ByteString -> Either String (Session, ByteString)
decodeSession keypair buffer = runGetState getSession buffer 0

decodePacket :: Session -> ByteString -> Either String (Plaintext, ByteString)
decodeMessage session buffer = runGetState getPacket buffer 0
