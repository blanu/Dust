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
        (idpath:_) -> fetch idpath
        otherwise      -> putStrLn "Usage: fetch [server-id]"

fetch :: String -> IO()
fetch idpath = do
    ids <- fetchIndex idpath
    putStrLn $ "Ids:" ++ (show ids)
    msgs <- fetchMessages idpath ids
    printMessages msgs

fetchIndex :: String -> IO([MessageID])
fetchIndex idpath = do
    let msg = Plaintext $ encode $ GetIndex
    response <- dustClient idpath msg

    let (Plaintext plaintext) = response
    let result = (decode plaintext)::(Either String ResultMessage)
    putStrLn $ "Result: " ++ (show result)
    putStrLn $ "Length: " ++ (show (B.length plaintext))

    return $ handleIndex response

handleIndex :: Plaintext -> [MessageID]
handleIndex (Plaintext plaintext) =
    let result = (decode plaintext)::(Either String ResultMessage)
    in case result of
        Right (IndexResult msgids) -> msgids
        otherwise -> []

printMessages :: [Message] -> IO()
printMessages [] = return ()
printMessages (id:ids) = do
    printMessage id
    printMessages ids

printMessage :: Message -> IO()
printMessage msg = do
    putStrLn $ "Messages:" ++ (show msg)

fetchMessages :: String -> [MessageID] -> IO([Message])
fetchMessages idpath [] = return ([])
fetchMessages idpath (id:ids) = do
    msg <- fetchMessage idpath id
    msgs <- fetchMessages idpath ids
    return (msg:msgs)

fetchMessage :: String -> MessageID -> IO(Message)
fetchMessage idpath id = do
    let msg = Plaintext $ encode $ GetMessages [id]
    response <- dustClient idpath msg
    let (msg:_) = handleMessages response
    return msg

handleMessages :: Plaintext -> [Message]
handleMessages (Plaintext plaintext) =
    let result = (decode plaintext)::(Either String ResultMessage)
    in case result of
        Right (MessagesResult msgs) -> msgs
        otherwise -> []

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"
