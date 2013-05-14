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

main = do
    eitherModel <- loadModel "traffic.model"
    case eitherModel of
        Left error -> do
            putStrLn "Error loading model"
            return ()
        Right model -> do
            let gen  = makeGenerator model
            args <- getArgs
            case args of
                ("--themes":idpath:_) -> fetchThemes gen idpath
                ("--themes":_) -> usage
                (idpath:_) -> fetch gen idpath
                otherwise      -> usage

usage :: IO()
usage = do
    putStrLn "Usage: fetch <--themes> [server-id]"

fetchThemes :: TrafficGenerator -> String -> IO()
fetchThemes gen idpath = do
    putStrLn "Fetching themes..."
    let msg = Plaintext $ encode $ GetThemes
    response <- dustClient gen idpath msg

    let (Plaintext plaintext) = response
    let result = (decode plaintext)::(Either String ResultMessage)
    putStrLn $ "Result: " ++ (show result)
    putStrLn $ "Length: " ++ (show (B.length plaintext))

    putStrLn $ show $ handleThemes response

handleThemes :: Plaintext -> [Theme]
handleThemes (Plaintext plaintext) =
    let result = (decode plaintext)::(Either String ResultMessage)
    in case result of
        Right (ThemesResult themes) -> themes
        otherwise -> []

fetch :: TrafficGenerator -> String -> IO()
fetch gen idpath = do
    ids <- fetchIndex gen idpath
    putStrLn $ "Ids:" ++ (show ids)
    msgs <- fetchMessages gen idpath ids
    printMessages msgs

fetchIndex :: TrafficGenerator -> String -> IO([MessageID])
fetchIndex gen idpath = do
    let msg = Plaintext $ encode $ GetIndex
    response <- dustClient gen idpath msg

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

fetchMessages :: TrafficGenerator -> String -> [MessageID] -> IO([Message])
fetchMessages gen idpath [] = return ([])
fetchMessages gen idpath (id:ids) = do
    msg <- fetchMessage gen idpath id
    msgs <- fetchMessages gen idpath ids
    return (msg:msgs)

fetchMessage :: TrafficGenerator -> String -> MessageID -> IO(Message)
fetchMessage gen idpath id = do
    let msg = Plaintext $ encode $ GetMessages [id]
    response <- dustClient gen idpath msg
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
