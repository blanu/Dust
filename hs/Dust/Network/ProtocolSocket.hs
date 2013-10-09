module Dust.Network.ProtocolSocket
(
    getSession,
    getPacket,
    encode
)
where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Dust.Crypto.Keys
import Dust.Crypto.DustCipher
import Dust.Core.CryptoProtocol
import Dust.Core.WireProtocolHandler

getSession :: ByteString -> Keypair -> Socket -> IO (Session, ByteString)
getSession buffer keypair sock = decodeBytes sock buffer (decodeSession keypair)

getPacket :: ByteString -> Session -> Socket -> IO (Plaintext, ByteString)
getPacket buffer session sock = decodeBytes sock buffer (decodePacket session)

decodeBytes :: Socket -> ByteString -> (ByteString -> Either String (a, ByteString)) -> IO (a, ByteString)
decodeBytes sock buffer decoder = do
    bs <- recv sock 1
    if (B.length bs) == 0
        then decodeBytes sock buffer decoder
        else do
            let buffer' = B.append buffer bs
            let eitherResult = decoder buffer'
            case eitherResult of
                Left error   -> decodeBytes sock buffer' decoder
                Right result -> return result

encode :: Session -> Plaintext -> Socket -> IO()
encode session plaintext sock = do
    let (Packets packets) = encodeMessage session plaintext
    sendPackets packets sock

sendPackets :: [ByteString] -> Socket -> IO()
sendPackets [] sock = return ()
sendPackets (bs:packets) sock = do
    sendAll sock bs
    sendPackets packets sock
