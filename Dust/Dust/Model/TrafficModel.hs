{-# LANGUAGE DeriveGeneric, DefaultSignatures, TemplateHaskell #-} -- For automatic generation of cereal put and get

module Dust.Model.TrafficModel
(
    ProtocolModel(..),
    TrafficModel(..),
    TrafficGenerator(..),
    Stream,
    newStream,
    loadModel,
    saveModel,
    generateDuration,
    generatePacketCount,
    generateLength
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

import Dust.Crypto.PRNG
import Dust.Model.PacketLength
import qualified Dust.Model.Content as C
import Dust.Model.Port
import Dust.Crypto.Cipher
import Dust.Crypto.Keys

data TrafficModel = TrafficModel {
    length  :: [Double],
    entropy  :: [Double],
    flow     :: Double,
    content  :: [Double],
    duration :: Double
  }
  deriving (Generic)

instance Serialize TrafficModel

data ProtocolModel = ProtocolModel {
    incoming :: TrafficModel,
    outgoing :: TrafficModel
  }
  deriving (Generic)

instance Serialize ProtocolModel

data Stream = Stream {
    _encoded   :: B.ByteString,
    _decoded   :: B.ByteString,
    _encrypted :: B.ByteString,
    _decrypted :: B.ByteString,
    _cipher    :: Handshake
  }

data Handshake =
    Begin Keypair
  | Sent Keypair
  | Complete Cipher

data TrafficGenerator = TrafficGenerator {
    _genModel :: ProtocolModel,
    _genPrng :: PRNG,
    _incomingStream :: Stream,
    _outgoingStream :: Stream
  }

makeLenses ''Stream
makeLenses ''TrafficGenerator

loadModel :: FilePath -> IO (Either String ProtocolModel)
loadModel path = do
    s <- B.readFile path
    return ((decode s)::(Either String ProtocolModel))

saveModel :: ProtocolModel -> FilePath -> IO ()
saveModel model path = do
  let s = encode model
  B.writeFile path s

newStream :: Keypair -> Stream
newStream keypair = Stream B.empty B.empty B.empty B.empty $ Begin keypair

generateDuration :: State TrafficGenerator Word16
generateDuration = do
  gen <- get
  let lambda = duration $ incoming $ _genModel $ gen
  let prng = gen ^. genPrng

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
  gen <- get
  let lambda = flow $ incoming $ _genModel $ gen
  let prng = gen ^. genPrng
  let (seed, prng') = randomWord16 prng
  genPrng .= prng'

  let p = poisson lambda
  let g = mkStdGen $ fromIntegral seed
  let result = evalState (runRVar (sample p) StdRandom) g
  rest <- generatePacketCounts (ms-1)
  return $ result : rest

generateLength :: State TrafficGenerator Word16
generateLength = do
  gen <- get
  let (mean:sigma:[]) = length $ incoming $ _genModel $ gen
  let prng = gen ^. genPrng
  let (result, prng') = normal' (mean,sigma) prng
  genPrng .= prng'
  return $ round result

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
