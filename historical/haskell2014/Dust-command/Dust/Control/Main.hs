import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.IO (stdin, stdout, hSetBuffering, BufferMode(..))
import System.IO.Error
import System.Environment (getArgs)
import Data.Binary.Get (runGetState)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Control.Monad.State.Lazy
import Debug.Trace

import Dust.Crypto.Keys
import Dust.Crypto.ECDH
import Dust.Crypto.Cipher
import Dust.Crypto.PRNG
import Dust.Model.TrafficModel
import Dust.Control.Command
import Dust.Control.Lazy
import Dust.Control.State
import Dust.Core.CryptoProtocol (Handshake(..))

main :: IO()
main = do
  args <- getArgs

  case args of
      (keypath:modelpath:[]) -> command keypath modelpath
      otherwise              -> do
        putStrLn "processes commands from stdin and returns results on stdout"
        putStrLn "Usage: command [keyfile] [modelfile]"

command :: String -> String -> IO()
command keypath modelpath = do
  rand <- newPRNG
  let (iv, rand') = createIV rand

  putStrLn "Loading keys..."
  (keypair, newKeys) <- ensureKeys keypath createKeypair

  if newKeys
    then putStrLn "Generating new keys..."
    else putStrLn "Loaded keys."

  eitherModel <- loadModel modelpath
  case eitherModel of
    Left _ -> putStrLn "Failure to load model"
    Right model -> do
      let gen  = TrafficGenerator model rand (newStream keypair) (newStream keypair) (Begin keypair) minimalConfig
      hSetBuffering stdin NoBuffering
      --hSetBuffering stdout NoBuffering

      -- Parse commands
      input <- byteSource
      let packets = packetize input
      let commands = catMaybes $ getRequests packets

      -- Process commands
      let maybeResults = evalState (mapM processCommand commands) gen

      -- Output results
      let results = catMaybes maybeResults
      putStrLn $ show $ results
      let resultStream = depacketize results
      putStrLn $ show resultStream

-- encoder :: Keypair -> IV -> TrafficGenerator ->
-- reencode :: Keypair -> IV -> TrafficGenerator -> (Plaintext -> IO(Plaintext)) -> Socket -> IO()
-- reencode keypair iv gen proxyAction sock = do
--     session@(Session _ otherPublic _) <- getSession gen keypair sock
--     plaintext <- getPacket gen session sock
--
--     putStrLn $ "Request:" ++ (show plaintext)
--
--     result <- proxyAction plaintext
--     let Plaintext resultBytes = result
--
--     putStrLn $ "Response:" ++ (show (B.length resultBytes))
--
--     let otherSession = makeSession keypair otherPublic iv
--     putSessionPacket gen otherSession result sock
--
--     return ()
