module Dust.Network.DustClient
(
 dustClient
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import Data.Binary.Put
import System.Environment (getArgs)
import System.IO.Error
import Network.Socket

import Dust.Crypto.Keys
import Dust.Crypto.Curve
import Dust.Core.Protocol
import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Network.TcpClient

dustClient :: Plaintext -> IO(Plaintext)
dustClient payload = do
    let host = "127.0.0.1"
    let port = 9001

    keypair <- createEphemeral
    public <- loadPublic "id.pub"
    iv <- createIV
    let session = makeSession keypair public iv

    client host port (handleRequest session payload)

handleRequest :: Session -> Plaintext -> Socket -> IO(Plaintext)
handleRequest session@(Session keypair _ _) payload sock = do
    putStrLn $ "Request:" ++ (show payload)

    putSession session sock
    putPacket session payload sock

    otherSession <- getSession keypair sock
    plaintext <- getPacket otherSession sock

    putStrLn $ "Response:" ++ (show plaintext)

    return plaintext
