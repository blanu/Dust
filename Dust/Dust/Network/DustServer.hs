module Dust.Network.DustServer
(
 dustServer
)
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.IO.Error (tryIOError)
import Data.Binary.Get (runGetState)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket

import Dust.Crypto.Keys
import Dust.Crypto.ECDH
import Dust.Core.WireProtocolHandler
import Dust.Network.TcpServer
import Dust.Crypto.DustCipher
import Dust.Model.TrafficModel
import Dust.Network.ProtocolSocket
import Dust.Core.CryptoProtocol
import Dust.Crypto.PRNG

dustServer :: TrafficGenerator -> (Plaintext -> IO(Plaintext)) -> IO()
dustServer gen proxyAction = do
    rand <- newPRNG

    putStrLn "Loading keys..."
    (keypair, newKeys, rand') <- ensureKeys rand

    if newKeys
        then putStrLn "Generating new keys..."
        else putStrLn "Loaded keys."

    let host = "0.0.0.0"
    let port = 6885

    rand <- newPRNG
    let (iv, rand'') = createIV rand'

    server host port (reencode keypair iv gen proxyAction)

ensureKeys :: DustPRNG -> IO (Keypair, Bool, DustPRNG)
ensureKeys rand = do
    result <- tryIOError loadKeypair
    case result of
        Left e -> do
            let (bytes, rand') = randomBytes 32 rand
            keys <- createKeypair
            saveKeypair keys
            return (keys, True, rand')
        Right keypair -> return (keypair, False, rand)

reencode :: Keypair -> IV -> TrafficGenerator -> (Plaintext -> IO(Plaintext)) -> Socket -> IO()
reencode keypair iv gen proxyAction sock = do
    (session, rest) <- getSession B.empty keypair sock
    let (Session _ otherPublic _ _) = session
    (plaintext, rest') <- getPacket rest session sock

    putStrLn $ "Request:" ++ (show plaintext)

    result <- proxyAction plaintext
    let Plaintext resultBytes = result

    putStrLn $ "Response:" ++ (show (B.length resultBytes))

    let otherSession = makeSession keypair otherPublic iv
    encode otherSession result sock

    return ()
