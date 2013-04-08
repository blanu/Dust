module Dust.Network.DustServer
(
 dustServer
)
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.IO.Error
import System.Entropy
import Data.Binary.Get (runGetState)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket

import Dust.Crypto.Keys
import Dust.Crypto.ECDH
import Dust.Core.Protocol
import Dust.Network.TcpServer
import Dust.Crypto.DustCipher
import Dust.Model.TrafficModel

dustServer :: TrafficGenerator -> (Plaintext -> IO(Plaintext)) -> IO()
dustServer gen proxyAction = do
    putStrLn "Loading keys..."
    (keypair, newKeys) <- ensureKeys

    if newKeys
        then putStrLn "Generating new keys..."
        else putStrLn "Loaded keys."

    let host = "0.0.0.0"
    let port = 6885

    iv <- createIV

    server host port (reencode keypair iv gen proxyAction)

ensureKeys :: IO (Keypair, Bool)
ensureKeys = do
    result <- try loadKeypair
    case result of
        Left e -> do
            entropy <- getEntropy 32
            let keys = createKeypair entropy
            saveKeypair keys
            return (keys, True)
        Right keypair -> return (keypair, False)

reencode :: Keypair -> IV -> TrafficGenerator -> (Plaintext -> IO(Plaintext)) -> Socket -> IO()
reencode keypair iv gen proxyAction sock = do
    session@(Session _ otherPublic _) <- getSession keypair sock
    plaintext <- getPacket session sock

    putStrLn $ "Request:" ++ (show plaintext)

    result <- proxyAction plaintext
    let Plaintext resultBytes = result

    putStrLn $ "Response:" ++ (show (B.length resultBytes))

    let otherSession = makeSession keypair otherPublic iv
    putSessionPacket gen otherSession result sock

    return ()
