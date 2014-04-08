module Dust.Network.ProtocolSocket
(
    getSession,
    getPacket,
    encode,
    encodeWithProgress
)
where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Debug.Trace

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
 --   putStrLn $ "Sending " ++ show packets
    sendPackets packets sock Nothing

encodeWithProgress :: Session -> Plaintext -> Socket -> (Int -> Int -> IO()) -> IO()
encodeWithProgress session plaintext sock callback = do
    let (Packets packets) = encodeMessage session plaintext
--    putStrLn $ "Sending " ++ show packets
    sendPackets packets sock (Just (callback $ foldl (+) 0 $ map B.length packets))

sendPackets :: [ByteString] -> Socket -> Maybe (Int -> IO()) -> IO()
sendPackets [] sock callback = return ()
sendPackets (bs:packets) sock callback = do
    sendAll sock bs
    case callback of
        Nothing -> sendPackets packets sock callback
        Just cb -> do
            cb $ B.length bs
            sendPackets packets sock callback
