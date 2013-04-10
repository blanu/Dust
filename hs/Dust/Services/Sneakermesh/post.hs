import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import Data.Binary.Put
import System.Environment (getArgs)
import System.IO.Error
import Network.Socket
import Data.Serialize
import Text.Printf (printf)
import System.Entropy

import Dust.Crypto.Keys
import Dust.Crypto.ECDH
import Dust.Core.Protocol
import Dust.Crypto.DustCipher
import Dust.Crypto.ECDSA
import Dust.Core.DustPacket
import Dust.Network.DustClient
import Dust.Services.Sneakermesh.Message
import Dust.Model.TrafficModel

main :: IO()
main = do
    args <- getArgs

    (keys, newkeys) <- ensureKeys

    case args of
        (filepath:idpath:_) -> post keys filepath idpath
        otherwise      -> putStrLn "Usage: post [message-file] [server-id]"

ensureKeys :: IO (Keypair, Bool)
ensureKeys = do
    result <- try loadSigningKeypair
    case result of
        Left e -> do
            entropy <- getEntropy 32
            let keys = createSigningKeypair entropy
            saveSigningKeypair keys
            return (keys, True)
        Right keypair -> return (keypair, False)

post :: Keypair -> FilePath -> FilePath -> IO()
post keys filepath idpath = do
    contents <- readFile filepath
    let msg = processArgs keys contents

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
    putStrLn "P0"
    result <- f
    putStrLn "P1"
    lots (x-1) f

doPost :: FilePath -> Plaintext -> (Plaintext -> B.ByteString) -> TrafficGenerator -> IO()
doPost idpath msg handler gen = do
    response <- dustClient gen idpath msg
    let result = handler response
    putStrLn $ "Response:" ++ (toHex result)

processArgs :: Keypair -> String -> Plaintext
processArgs keys arg =
    let plainMessage = encode $ PutMessage $ pack arg
        signedMessage = encode $ sign plainMessage keys
    in Plaintext signedMessage

handler :: Plaintext -> B.ByteString
handler (Plaintext plaintext) = plaintext

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"
