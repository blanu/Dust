import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import System.IO.Error
import System.Entropy

import Dust.Crypto.Keys
import Dust.Crypto.Curve
import Dust.Core.Protocol
import Dust.Network.TcpServer
import Dust.Crypto.DustCipher

main = do
    putStrLn "Loading keys..."
    (keypair, newKeys) <- ensureKeys

    if newKeys
        then putStrLn "Generating new keys..."
        else putStrLn "Loaded keys."

    let host = "127.0.0.1"
    let port = 9001

    iv <- createIV
    result <- server host port (reencode keypair iv)

    putStrLn (show result)

reencode :: Keypair -> IV -> ByteString -> IO(ByteString)
reencode keypair iv inputBytes =
    let Conversation public _ = deserializeConversation inputBytes
        payload = decode keypair inputBytes
    in return(encode keypair public iv payload)

ensureKeys :: IO (Keypair, Bool)
ensureKeys = do
    result <- try loadKeypair
    case result of
        Left e -> do
            entropy <- getEntropy 32
            let keys = createKeypair entropy
            saveKeypair keys
            return (keys, True)
        Right keypair -> return (keypair, False)
