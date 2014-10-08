module Dust.Network.ProtocolSocket
(
    getSession,
    getPacket,
    encode,
    encodeWithProgress
)
where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, send)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Control.Monad (when)
import Debug.Trace

import Dust.Crypto.Keys
import Dust.Crypto.Cipher
import Dust.Core.CryptoProtocol
import Dust.Core.WireProtocolHandler

getSession :: ByteString -> Keypair -> Socket -> IO (Session, ByteString)
getSession buffer keypair sock = decodeBytes sock buffer (decodeSession keypair)

getPacket :: ByteString -> Session -> Socket -> IO (Plaintext, ByteString)
getPacket buffer session sock = decodeBytes sock buffer (decodePacket session)

decodeBytes :: Socket -> ByteString -> (ByteString -> Either String (a, ByteString)) -> IO (a, ByteString)
decodeBytes sock buffer decoder = do
--    bs <- recv sock 1000
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
    sendPackets packets sock 0 Nothing

encodeWithProgress :: Session -> Plaintext -> Socket -> (Int -> Int -> IO()) -> IO()
encodeWithProgress session plaintext sock callback = do
    let (Packets packets) = encodeMessage session plaintext
--    putStrLn $ "Sending " ++ show packets
    sendPackets packets sock 0 (Just (callback $ foldl (+) 0 $ map B.length packets))

sendPackets :: [ByteString] -> Socket -> Int -> Maybe (Int -> IO()) -> IO()
sendPackets [] sock count callback = return ()
sendPackets (bs:packets) sock count callback = do
    sent <- sendAll sock bs count callback
    sendPackets packets sock (count+sent) callback

sendAll :: Socket -> ByteString -> Int -> Maybe (Int -> IO()) -> IO (Int)
sendAll sock bs count callback = do
    sent <- send sock bs
--    putStrLn $ "Sent " ++ (show sent)
    case callback of
        Nothing -> do
            if sent < B.length bs
                then sendAll sock (B.drop sent bs) (count+sent) callback
                else return sent
        Just cb -> do
            cb (count+sent)
            if sent < B.length bs
                then sendAll sock (B.drop sent bs) (count+sent) callback
                else return sent
