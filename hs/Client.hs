import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import System.IO.Error

import Dust.Crypto.Keys
import Dust.Crypto.Curve
import Dust.Core.Protocol
import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Network.TcpClient

main = do
    args <- getArgs

    case args of
        (arg:_) -> do
            result <- fetch arg
            putStrLn (show result)
        [] -> putStrLn "Usage: dust-client [arg]"

fetch :: String -> IO(ByteString)
fetch arg = do
    keypair <- createEphemeral
    public <- loadPublic "id.pub"
    iv <- createIV
    let payload = pack arg

    let sendBytes = encode keypair public iv payload

    putStrLn (show sendBytes)

    let host = "127.0.0.1"
    let port = 9001

    result <- client host port sendBytes
    let resultPayload = decode keypair result

    return resultPayload
