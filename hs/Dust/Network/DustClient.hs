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
import Dust.Crypto.ECDH
import Dust.Core.Protocol
import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Network.TcpClient
import Dust.Model.TrafficModel

dustClient :: FilePath -> Plaintext -> IO(Plaintext)
dustClient idpath payload = do
    eitherModel <- loadModel "traffic.model"
    case eitherModel of
        Left error -> do
            putStrLn "Error loading model"
            return (Plaintext (pack ""))
        Right model -> do
            let gen  = makeGenerator model

            let host = "166.78.129.122"
            let port = 6885

            keypair <- createEphemeral
            public <- loadPublic idpath
            iv <- createIV
            let session = makeSession keypair public iv

            client host port (handleRequest gen session payload)

handleRequest :: TrafficGenerator -> Session -> Plaintext -> Socket -> IO(Plaintext)
handleRequest gen session@(Session keypair _ _) payload sock = do
    putStrLn $ "Request:" ++ (show payload)

    putSessionPacket gen session payload sock

    otherSession <- getSession keypair sock
    plaintext <- getPacket otherSession sock

    putStrLn $ "Response:" ++ (show plaintext)

    return plaintext
