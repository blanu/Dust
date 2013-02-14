import Network.Socket
import Control.Monad (forever)
import Control.Concurrent

port = 9876
host = "127.0.0.1"

main = withSocketsDo $ do
        s <- socket AF_INET Datagram defaultProtocol
        hostAddr <- inet_addr host
        thread <- forkIO $ receiveMessages s
        forever $ do
                msg <- getLine
                sendTo s msg (SockAddrInet port hostAddr)
        killThread thread
        sClose s
        return()

receiveMessages :: Socket -> IO ()
receiveMessages socket = forever $ do
        msg <-recv socket 1024
        putStrLn msg

