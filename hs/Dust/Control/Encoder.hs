module Dust.Control.Encoder
(
 dustServer
)
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.IO (stdin, stdout)
import System.IO.Error
import Data.Binary.Get (runGetState)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket
import Crypto.Threefish.Random

import Dust.Crypto.Keys
import Dust.Crypto.ECDH
import Dust.Core.Protocol
import Dust.Network.TcpServer
import Dust.Crypto.DustCipher
import Dust.Model.TrafficModel

main :: IO
main = do
   args <- getArgs

    case args of
        (keypath:modelpath:[]) -> convert pcappath protocol port pspath
        otherwise                         -> do
          putStrLn "encodes bytes from stdin to a series of packets on stdout"
          putStrLn "Usage: encode [keyfile] [modelfile]"

    rand <- newSkeinGen
    let (iv, rand') = createIV rand

    putStrLn "Loading keys..."
    (keypair, newKeys, rand'') <- ensureKeys rand'

    if newKeys
        then putStrLn "Generating new keys..."
        else putStrLn "Loaded keys."

    eitherObs <- loadObservations "traffic.model"
    case eitherObs of
        Left error -> putStrLn "Error loading model"
        Right obs -> do
            let model = makeModel obs
            let gen  = makeGenerator model
            encode $ encoder keypair iv gen

ensureKeys :: SkeinGen -> IO (Keypair, Bool, SkeinGen)
ensureKeys rand = do
    result <- tryIOError loadKeypair
    case result of
        Left e -> do
            let (bytes, rand') = randomBytes 32 rand
            let keys = createKeypair bytes
            saveKeypair keys
            return (keys, True, rand')
        Right keypair -> return (keypair, False, rand)

encode :: IO()
encode = do
    hSetBuffering stdin NoBuffering
    encodeWithBuffer B.empty

encodeWithBuffer :: ByteString -> IO()
encodeWithBuffer buffer = do
    eof <- isEOF
    if eof
        then return ()
        else do
            ready <- hWaitForInput stdin 1000
            if ready
                then encodeWithBuffer $ readAndEncode buffer
                else encodeWithBuffer buffer

readAndEncode :: ByteString -> IO()
readAndEncode buffer = do
    b <- B.hGet stdin 1
    let newBuffer = B.append buffer b

    let (packets, rest) = encodeBuffer newBuffer
    writePackets packets
    encodeWithBuffer rest

encodeBuffer :: ByteString -> (Packets, ByteString)
encodeBuffer buffer = do


writePackets :: Packets -> IO()
writePackets packets = do



encoder :: Keypair -> IV -> TrafficGenerator ->
reencode :: Keypair -> IV -> TrafficGenerator -> (Plaintext -> IO(Plaintext)) -> Socket -> IO()
reencode keypair iv gen proxyAction sock = do
    session@(Session _ otherPublic _) <- getSession gen keypair sock
    plaintext <- getPacket gen session sock

    putStrLn $ "Request:" ++ (show plaintext)

    result <- proxyAction plaintext
    let Plaintext resultBytes = result

    putStrLn $ "Response:" ++ (show (B.length resultBytes))

    let otherSession = makeSession keypair otherPublic iv
    putSessionPacket gen otherSession result sock

    return ()
