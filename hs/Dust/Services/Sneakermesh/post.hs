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
import Dust.Model.TrafficModel

main :: IO()
main = do
    args <- getArgs

    case args of
        (filepath:idpath:_) -> post filepath idpath
        otherwise      -> putStrLn "Usage: post [message-file] [server-id]"

post :: FilePath -> FilePath -> IO()
post filepath idpath = do
    contents <- readFile filepath
    let msg = processArgs contents

    eitherModel <- loadModel "traffic.model"
    case eitherModel of
        Left error -> do
            putStrLn "Error loading model"
            return ()
        Right model -> do
            let gen  = makeGenerator model
            lots 1000 (doPost idpath msg handler gen)

lots :: Int -> IO() -> IO()
lots 0 f = return ()
lots x f = do
    result <- f
    lots (x-1) f

doPost :: FilePath -> Plaintext -> (Plaintext -> B.ByteString) -> TrafficGenerator -> IO()
doPost idpath msg handler gen = do
    response <- dustClient gen idpath msg
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
