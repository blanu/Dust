import Network.Socket (withSocketsDo, socket, inet_addr, bindSocket, SockAddr(SockAddrInet), sClose, Family(AF_INET), SocketType(Datagram), defaultProtocol)
import Network.Socket.ByteString (recvFrom, sendTo)
import Data.Char (toUpper)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Crypto.Classes
import Crypto.Skein

port = 9876
host = "0.0.0.0"

type Host = SockAddr

main = withSocketsDo $ do
        sock <- initSocket host port
        processLoop sock
        sClose sock

initSocket host port = do
        s <- socket AF_INET Datagram defaultProtocol
        bindAddr <- inet_addr host
        bindSocket s (SockAddrInet port bindAddr)
        return s

processLoop sock = do
        forever (echo sock transform)

forever f = do
        f
        forever f

echo socket transform = do
        (input, host) <- recvFrom socket 1024
        let output = transform input
        sendTo socket output host
        return ()

digest :: ByteString -> Skein_512_512
digest bs = hash' bs::Skein_512_512

transform = encode . digest
