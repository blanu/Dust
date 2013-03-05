import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP

import Dust.Network.DustServer

main = dustServer webProxy

webProxy :: Plaintext -> IO(Plaintext)
webProxy (Plaintext inputBytes) = do
    let url = unpack inputBytes
    response <- simpleHTTP (getRequest url) >>= getResponseBody
    return $ Plaintext (pack response)
