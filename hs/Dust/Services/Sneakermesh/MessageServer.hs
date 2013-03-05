import System.IO
import System.FilePath
import System.Directory
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Crypto.Classes
import Crypto.Skein
import Data.Serialize
import Text.Printf (printf)

import Dust.Crypto.DustCipher
import Dust.Network.DustServer
import Dust.Services.Sneakermesh.Message

main :: IO()
main = dustServer messageServer

messageServer :: Plaintext -> IO(Plaintext)
messageServer (Plaintext inputBytes) = do
    let command = (decode inputBytes)::(Either String Command)
    case command of
        Left error -> do
            putStrLn $ "Error!" ++ show(error)
            return $ Plaintext (pack "Error!")
        Right cmd -> do
            case cmd of
                PutMessage msg -> do
                    result <- putMessage msg
                    return $ Plaintext result
                GetIndex -> do
                    result <- getIndex
                    return $ Plaintext $ encode result
                GetMessages msgids -> do
                    msgs <- getMessages msgids
                    return $ Plaintext $ encode $ MessagesResult msgs

putMessage :: B.ByteString -> IO(B.ByteString)
putMessage inputBytes = do
    putStrLn "hashing"
    let filehash = digest inputBytes

    putStrLn "writing"
    let filepath = "sneakermesh"
    createDirectoryIfMissing False filepath
    let filename = filepath </> toHex filehash
    B.writeFile filename inputBytes

    let indexpath = filepath </> "index"
    B.appendFile indexpath filehash

    return filehash

getIndex :: IO(ResultMessage)
getIndex = do
    let filepath = "sneakermesh"
    let indexpath = filepath </> "index"
    index <- B.readFile indexpath
    return $ parseIndex index

parseIndex :: B.ByteString -> ResultMessage
parseIndex bytes =
    let messages = parseMessageIDs bytes
    in IndexResult messages

parseMessageIDs :: B.ByteString -> [MessageID]
parseMessageIDs bytes =
    let (msgid, rest) = B.splitAt 64 bytes
    in if (B.null rest)
       then [MessageID msgid]
       else [MessageID msgid] ++ parseMessageIDs rest

getMessages :: [MessageID] -> IO([Message])
getMessages [] = return ([])
getMessages (msgid:msgids) = do
    msg <- getMessage msgid
    msgs <- getMessages msgids
    return (msg : msgs)

getMessage :: MessageID -> IO(Message)
getMessage (MessageID msgid) = do
    let filepath = "sneakermesh"
    let filename = filepath </> toHex msgid
    msg <- B.readFile filename
    return msg

digest :: B.ByteString -> B.ByteString
digest bs =
    let h = hash' bs :: Skein_512_512
    in encode h

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"
