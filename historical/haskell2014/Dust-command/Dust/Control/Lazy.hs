module Dust.Control.Lazy
(
  byteSource,
  packetize,
  depacketize,
  getRequests
)
where

import Data.ByteString.Char8 (pack, unpack)
import System.IO (stdin, stdout)
import System.IO.Error
import Data.Binary.Get (runGet, runGetState)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

import Dust.Control.Protocol
import Dust.Control.State

byteSource :: IO BL.ByteString
byteSource = BL.getContents

--------

packetize :: BL.ByteString -> [BL.ByteString]
packetize stream = do
  if BL.null stream
    then []
    else do
      let (lenbs, stream') = BL.splitAt 2 stream
      let len = fromIntegral $ runGet getPacketLength lenbs
      let (bs, stream'') = BL.splitAt len stream'
      bs : packetize stream''

depacketize :: [BL.ByteString] -> BL.ByteString
depacketize [] = BL.empty
depacketize (packet:packets) = do
  let len = BL.length packet
  let lenbs = runPut (putPacketLength (fromIntegral len))
  let chunk = BL.append lenbs packet
  BL.append chunk (depacketize packets)

--------

getRequests :: [BL.ByteString] -> [Maybe ControlRequest]
getRequests [] = []
getRequests (packet:packets) = do
  let request = runGet getControlRequest packet
  request : getRequests packets
