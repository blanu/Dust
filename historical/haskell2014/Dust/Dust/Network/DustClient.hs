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
import Crypto.Threefish.Random

import Dust.Crypto.Keys
import Dust.Crypto.ECDH
import Dust.Core.Protocol
import Dust.Crypto.Cipher
import Dust.Core.DustPacket
import Dust.Network.TcpClient
import Dust.Model.TrafficModel

dustClient :: TrafficGenerator -> FilePath -> Plaintext -> IO(Plaintext)
dustClient gen idpath payload = do
--    let host = "166.78.129.122"
    let host = "127.0.0.1"
    let port = 6885

    rand <- newSkeinGen
    let (iv, rand') = createIV rand

    keypair <- createKeypair
    public <- loadPublic idpath
    let session = makeSession keypair public iv

    client host port (handleRequest gen session payload)

handleRequest :: TrafficGenerator -> Session -> Plaintext -> Socket -> IO(Plaintext)
handleRequest gen session@(Session keypair _ _) payload sock = do
    putStrLn $ "Request:" ++ (show payload)

    putSessionPacket gen session payload sock

    otherSession <- getSession gen keypair sock
    plaintext <- getPacket gen otherSession sock

    putStrLn $ "Response:" ++ (show plaintext)

    return plaintext
