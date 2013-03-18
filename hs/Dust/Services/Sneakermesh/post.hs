import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import Data.Binary.Put
import System.Environment (getArgs)
import System.IO.Error
import Network.Socket
import Data.Serialize
import Text.Printf (printf)

import Dust.Crypto.Keys
import Dust.Crypto.Curve
import Dust.Core.Protocol
import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Network.DustClient
import Dust.Services.Sneakermesh.Message

main = do
    args <- getArgs

    case args of
        (filepath:idpath:_) -> post filepath idpath
        otherwise      -> putStrLn "Usage: post [message-file] [server-id]"

<<<<<<< HEAD
post filepath idpath = do
    contents <- readFile filepath
    let msg = processArgs contents
    response <- dustClient idpath msg
=======
fetch arg = do
    msg <- processArgs arg
    response <- dustClient msg
>>>>>>> 2102c005d3e660dcf879c169c7450d3ece13b23c
    let result = handler response
    putStrLn $ "Response:" ++ (toHex result)

processArgs :: String -> IO(Plaintext)
processArgs arg = do
    payload <- B.readFile arg
    let message = encode $ PutMessage $ payload
    return $ Plaintext message

handler :: Plaintext -> B.ByteString
handler (Plaintext plaintext) = plaintext

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"
