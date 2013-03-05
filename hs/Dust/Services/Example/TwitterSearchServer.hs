import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP

import Dust.Crypto.DustCipher
import Dust.Network.DustServer

main = dustServer twitterSearch

twitterSearch :: Plaintext -> IO(Plaintext)
twitterSearch (Plaintext inputBytes) = do
    let url = "http://search.twitter.com/search.json?q=" ++ unpack inputBytes
    response <- simpleHTTP (getRequest url) >>= getResponseBody
    return $ Plaintext (pack response)
