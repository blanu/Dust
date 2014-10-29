{-# LANGUAGE DeriveGeneric, DefaultSignatures, TemplateHaskell #-} -- For automatic generation of cereal put and get

module Dust.Model.TrafficModel
(
  Config(..),
  ProtocolModel(..),
  TrafficModel(..),
  TrafficGenerator(..),
  Stream,
  minimalConfig,
  ultimateConfig,
  newStream,
  loadModel,
  saveModel,
  generateDuration,
  generatePacketCount,
  generateLength,
  encodedReady,
  decodedReady,
  putEncoded,
  putDecoded,
  getEncoded,
  getDecoded,
  generatePadding,
  config,
  padding
)
where

import Prelude hiding (length)
import GHC.Generics
import Data.Serialize (Serialize, encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Control.Monad.State.Lazy (State(..), get, put)
import Control.Lens
import Control.Lens.TH
import Data.Random.Normal
import Data.Random.Sample
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Exponential
import Data.Random.Source.Std
import System.Random
import Control.Monad.State.Lazy
import Debug.Trace

import Dust.Crypto.PRNG
import Dust.Model.PacketLength
import qualified Dust.Model.Content as C
import Dust.Model.Port
import Dust.Crypto.Cipher
import Dust.Crypto.Keys
import Dust.Core.CryptoProtocol hiding (Stream)
import Dust.Model.Content

data TrafficModel = TrafficModel {
    _length   :: [Double],
    _entropy  :: [Double],
    _flow     ::  Double,
    _content  :: [Double],
    _duration ::  Double
  }
  deriving (Generic)

instance Serialize TrafficModel

data Config = Config {
    _padding    :: Bool,
    _encryption :: Bool,
    _huffman    :: Bool
  }
  deriving (Show)

data ProtocolModel = ProtocolModel {
    _incoming :: TrafficModel,
    _outgoing :: TrafficModel
  }
  deriving (Generic)

instance Serialize ProtocolModel

data Stream = Stream {
    _encoded   :: B.ByteString,
    _decoded   :: B.ByteString,
    _encrypted :: B.ByteString
  }

data TrafficGenerator = TrafficGenerator {
    _genModel       :: ProtocolModel,
    _genPrng        :: PRNG,
    _incomingStream :: Stream,
    _outgoingStream :: Stream,
    _handshake      :: Handshake,
    _config         :: Config
  }

makePrisms ''Handshake
makeLenses ''Handshake
makeLenses ''Stream
makeLenses ''ProtocolModel
makeLenses ''TrafficModel
makeLenses ''TrafficGenerator
makeLenses ''Config

loadModel :: FilePath -> IO (Either String ProtocolModel)
loadModel path = do
    s <- B.readFile path
    return ((decode s)::(Either String ProtocolModel))

saveModel :: ProtocolModel -> FilePath -> IO ()
saveModel model path = do
  let s = encode model
  B.writeFile path s

minimalConfig :: Config
minimalConfig = Config False False False

ultimateConfig :: Config
ultimateConfig = Config True True True

newStream :: Keypair -> Stream
newStream keypair = Stream B.empty B.empty B.empty

generateDuration :: State TrafficGenerator Word16
generateDuration = do
  lambda <- use $ genModel.incoming.duration
  prng <- use genPrng

  let (seed, prng') = randomWord16 prng
  genPrng .= prng'

  let e = exponential lambda
  let g = mkStdGen $ fromIntegral seed
  let result = evalState (runRVar (sample e) StdRandom) g

  genPrng .= prng'
  return $ round result

generatePacketCount :: Word16 -> State TrafficGenerator Word16
generatePacketCount ms = do
  counts <- generatePacketCounts ms
  let result = foldl (+) 0 counts
  return result

generatePacketCounts :: Word16 -> State TrafficGenerator [Word16]
generatePacketCounts 0 = return []
generatePacketCounts ms = do
  lambda <- use $ genModel.incoming.flow
  prng <- use genPrng
  let (seed, prng') = randomWord16 prng
  genPrng .= prng'

  let p = poisson lambda
  let g = mkStdGen $ fromIntegral seed
  let result = evalState (runRVar (sample p) StdRandom) g
  rest <- generatePacketCounts (ms-1)
  return $ result : rest

generateLength :: State TrafficGenerator Word16
generateLength = do
  (mean:sigma:[]) <- use $ genModel.incoming.length
  prng <- use genPrng
  let (result, prng') = normal' (mean,sigma) prng
  genPrng .= prng'
  return $ round result

generatePadding :: Int -> State TrafficGenerator B.ByteString
generatePadding 0   = return B.empty
generatePadding len = do
  dist <- use $ genModel.incoming.content
  prng <- use genPrng
  let (w16, prng') = runState (nextLength dist) prng
  let w8 = (fromIntegral w16) :: Word8
  let b = B.singleton w8
  bs <- generatePadding (len-1)
  genPrng .= prng'
  return $ B.append b bs

encodedReady :: State TrafficGenerator Word16
encodedReady = do
  buffer <- use $ outgoingStream.encoded
  return $ fromIntegral $ B.length buffer

decodedReady :: State TrafficGenerator Word16
decodedReady = do
  buffer <- use $ incomingStream.decoded
  return $ fromIntegral $ B.length buffer

putEncoded :: B.ByteString -> State TrafficGenerator ()
putEncoded bs = do
  incomingStream.encoded %= flip B.append bs
  processEncoded

putDecoded :: B.ByteString -> State TrafficGenerator ()
putDecoded bs = do
  outgoingStream.decoded %= flip B.append bs
  processDecoded

getEncoded :: Word16 -> State TrafficGenerator (Maybe B.ByteString)
getEncoded 0 = return Nothing
getEncoded len = do
  let ilen = fromIntegral len
  buffer <- use $ outgoingStream.encoded
  let blen = B.length buffer
  if ilen <= blen
    then do
      let (result, buffer') = B.splitAt ilen buffer
      outgoingStream.encoded .= buffer'
      return $ Just result
    else return Nothing

getDecoded :: Word16 -> State TrafficGenerator (Maybe B.ByteString)
getDecoded 0 = return Nothing
getDecoded len = do
  let ilen = fromIntegral len
  buffer <- use $ incomingStream.decoded
  let blen = B.length buffer
  if trace (show ilen ++ " " ++ show blen) $ ilen <= blen
    then do
      let (result, buffer') = B.splitAt ilen buffer
      incomingStream.decoded .= buffer'
      return $ Just result
    else return Nothing

processEncoded :: State TrafficGenerator ()
processEncoded = do
  ebuff <- use $ incomingStream.encoded
  dbuff <- use $ incomingStream.decoded
  ciphertext <- use $ incomingStream.encrypted
  shake <- use handshake
  decrypting <- use $ config.encryption
  decoding <- use $ config.huffman
  probs <- use $ genModel.incoming.content
  let tree = treeFromProbs probs

  -- If there is any new encoded data
  when ((B.length ebuff) > 0) $ do
    -- Try to decode encoded to possibly get encrypted
    let (dec, rest) = if decoding then huffmanDecode tree ebuff else (ebuff, B.empty)
    -- Decoding yields the decoded data and the remainder encoded data which could not be decoded
    when ((B.length dec) > 0) $ do
      -- If decoding succeeded, we need to decrypt
      -- Remember the remainder encoded data for next time
      incomingStream.encoded .= rest
      -- The new decoded data is encrypted. Append it to the existing encrypted data
      let ciphertext' = B.append ciphertext dec
      -- We might not be able to decrypt this time, so remember the encrypted data for next time
      incomingStream.encrypted .= ciphertext'

      -- If there is any new encrypted data
      when ((B.length ciphertext') > 0) $ do
        -- Decrypt encrypted to get new decoded, new encrypted buffer, and new handshake
        let (plaintext, ciphertext'', shake') = if decrypting then performDecryption ciphertext' shake else (ciphertext', B.empty, shake)
        -- There might be some encrypted data left over, so remember it for next time
        incomingStream.encrypted .= ciphertext''
        -- Remember the state of the handshake for next time
        handshake .= shake'

        -- If there is any new decoded data
        when ((B.length plaintext) > 0) $ do
          -- Buffer it for later
          incomingStream.decoded %= flip B.append plaintext

processDecoded :: State TrafficGenerator ()
processDecoded = do
  ebuff <- use $ outgoingStream.encoded
  dbuff <- use $ outgoingStream.decoded
  ciphertext <- use $ outgoingStream.encrypted
  shake <- use $ handshake
  encrypting <- use $ config.encryption
  encoding <- use $ config.huffman
  probs <- use $ genModel.incoming.content
  let tree = treeFromProbs probs

  -- If there is any new decoded data,
  when ((B.length dbuff) > 0) $ do
    -- Encrypt decoded to get new encrypted, new decoded buffer, new handshake
    let (result, dbuff', shake') = if encrypting then performEncryption dbuff shake else (dbuff, B.empty, shake)
    -- Append the new encrypted to the encrypted buffer
    let ciphertext' = B.append ciphertext result

    -- Encryption might have failed, so remember everything for next time
    -- Remember encrypted buffer for next time
    outgoingStream.encrypted .= ciphertext'
    -- Remember encoded buffer for next time
    outgoingStream.encoded .= dbuff'
    -- Remember handshake for next time
    handshake .= shake'

    -- If there is any new encrypted data,
    when ((B.length ciphertext') > 0) $ do
      -- Encode encrypted to get new encoded and append to encoded buffer
      -- Encoding will always succeed, so there is no remainder
      let enc = if encoding then huffmanEncode tree ciphertext' else ciphertext'
      outgoingStream.encoded %= flip B.append enc
      -- Erase the encrypted data. We encoded 100% of the encrypted data.
      outgoingStream.encrypted .= B.empty

huffmanEncode :: ContentModel -> B.ByteString -> B.ByteString
huffmanEncode tree bs = encodeContent tree bs

huffmanDecode :: ContentModel -> B.ByteString -> (B.ByteString, B.ByteString)
huffmanDecode tree bs = do
  let dec = decodeContent tree bs
  (dec, B.empty)

-- makeGenerator :: TrafficModel -> TrafficGenerator
-- makeGenerator model = undefined
--   let (PacketLengthModel probs) = length model
--       lengthGen = nextLength probs
--       (C.ContentModel tree) = content model
--       enc = C.encodeContent tree
--       dec = C.decodeContent tree
--       (PortModel portList) = ports model
--       portGen = nextPort portList
--   in TrafficGenerator lengthGen enc dec portGen
