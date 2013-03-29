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
import Dust.Crypto.ECDH
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

post filepath idpath = do
    contents <- readFile filepath
    let msg = processArgs contents
    response <- dustClient idpath msg
    let result = handler response
    putStrLn $ "Response:" ++ (toHex result)

processArgs :: String -> Plaintext
processArgs arg =
    let message = encode $ PutMessage $ pack arg
    in Plaintext message

handler :: Plaintext -> B.ByteString
handler (Plaintext plaintext) = plaintext

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"
