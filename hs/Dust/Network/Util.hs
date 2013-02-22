module Dust.Network.Util
(
 recvAll
)
where

import Data.ByteString
import qualified Data.ByteString as B
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

recvAll :: Socket -> IO(ByteString)
recvAll sock = do
    input <- recv sock 4096
    if B.null input
        then return (input)
        else do
            next <- recvAll sock
            return (B.append input next)
