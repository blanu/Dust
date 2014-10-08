{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.WireProtocolHandler
(
 Packets(..),
 encodeMessage,
 decodeMessage,
 decodeSession,
 decodePacket
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Int
import Data.Serialize.Put (Put, runPut)
import Data.Serialize.Get (runGetState)
import Debug.Trace
import qualified Data.ByteString.Base64 as B64

import Dust.Core.DustPacket
import Dust.Crypto.Cipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Model.TrafficModel
import Dust.Core.CryptoProtocol
import Dust.Core.WireProtocol

data Packets = Packets [ByteString]

encodeMessage :: Session -> Plaintext -> Packets
encodeMessage session plaintext = do
  let header = runPut (putSession session)
  let packet = runPut (putPacket session plaintext)
  Packets $ splitPackets $ B.append header packet

splitPackets :: ByteString -> [ByteString]
splitPackets bs = do
    if B.length bs < 1000
        then [bs]
        else B.take 1000 bs : (splitPackets $ B.drop 1000 bs)

showSession :: Session -> String
showSession (Session (Keypair (PublicKey public ) _) _ _ _) = show $ B64.encode public

decodeMessage :: Keypair -> ByteString -> Either String (Plaintext, ByteString)
decodeMessage keypair buffer = do
  let eitherSession = decodeSession keypair buffer
  case eitherSession of
    Left error -> Left error
    Right (session, rest) -> decodePacket session rest

decodeSession :: Keypair -> ByteString -> Either String (Session, ByteString)
decodeSession keypair buffer = runGetState (getSession keypair) buffer 0

decodePacket :: Session -> ByteString -> Either String (Plaintext, ByteString)
decodePacket session buffer = runGetState (getPacket session) buffer 0
