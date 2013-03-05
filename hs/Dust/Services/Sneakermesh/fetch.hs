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
    ids <- fetchIndex
    putStrLn $ "Response:" ++ (show ids)
    msgs <- fetchMessages ids
    putStrLn $ "Response:" ++ (show msgs)

handleIndex :: Plaintext -> [MessageID]
handleIndex (Plaintext plaintext) =
    let result = (decode plaintext)::(Either String ResultMessage)
    in case result of
        Right (IndexResult msgids) -> msgids
        otherwise -> []

fetchIndex :: IO([MessageID])
fetchIndex = do
    let msg = Plaintext $ encode $ GetIndex
    response <- dustClient msg
    return $ handleIndex response

fetchMessages :: [MessageID] -> IO([Message])
fetchMessages ids = do
    let msg = Plaintext $ encode $ GetMessages ids
    response <- dustClient msg
    return $ handleMessages response

handleMessages :: Plaintext -> [Message]
handleMessages (Plaintext plaintext) =
    let result = (decode plaintext)::(Either String ResultMessage)
    in case result of
        Right (MessagesResult msgs) -> msgs
        otherwise -> []

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"
