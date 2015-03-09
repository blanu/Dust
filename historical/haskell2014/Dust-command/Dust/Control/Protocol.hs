module Dust.Control.Protocol
(
  ControlRequest(..),
  getPacketLength,
  getPacket,
  putPacketLength,
  putPacket,
  getControlRequest
)
where

import Data.Binary (encode, decode, getWord8, putWord8)
import Data.Binary.Get (Get(..), runGetState, getWord16be, getByteString)
import Data.Binary.Put (Put(..), runPut, putWord16be, putByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word16)

data ControlRequest =
    Duration
  | PacketCount Word16
  | EncodedReady
  | PutDecoded B.ByteString
  | GetEncoded Word16
  | DecodedReady
  | PutEncoded B.ByteString
  | GetDecoded Word16
  deriving (Show)

getPacketLength :: Get Int
getPacketLength = do
  len <- getWord16be
  return (fromIntegral len)

getPacket :: Get B.ByteString
getPacket = do
  len <- getWord16be
  bs <- getByteString (fromIntegral len)
  return bs

putPacket :: B.ByteString -> Put
putPacket bs = do
  let len = B.length bs
  putWord16be (fromIntegral len)
  putByteString bs

putPacketLength :: Int -> Put
putPacketLength len = putWord16be (fromIntegral len)

getControlRequest :: Get (Maybe ControlRequest)
getControlRequest = do
  command <- getWord8
  case command of
    0x01 -> return $ Just EncodedReady
    0x02 -> do
      len <- getWord16be
      arg <- getByteString (fromIntegral len)
      return $ Just $ PutDecoded arg
    0x03 -> do
      arg <- getWord16be
      return $ Just $ GetEncoded arg

    0x11 -> return $ Just DecodedReady
    0x12 -> do
      len <- getWord16be
      arg <- getByteString (fromIntegral len)
      return $ Just $ PutEncoded arg
    0x13 -> do
      arg <- getWord16be
      return $ Just $ GetDecoded arg

    0x21 -> return $ Just $ Duration
    0x22 -> do
      arg <- getWord16be
      return $ Just $ PacketCount arg

    otherwise -> return Nothing
