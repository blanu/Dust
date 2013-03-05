module Dust.Network.Util
(
 recvAll
)
where

import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

recvAll :: Socket -> IO(BL.ByteString)
recvAll sock = do
    list <- recvList sock
    return (BL.fromChunks list)

recvList :: Socket -> IO([B.ByteString])
recvList sock = do
    input <- recv sock 4096
    if B.null input
        then return ([input])
        else do
            next <- recvList sock
            return (input:next)
