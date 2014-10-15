{-# LANGUAGE DeriveGeneric, DefaultSignatures, TemplateHaskell #-} -- For automatic generation of cereal put and get

module Dust.Model.TrafficModel
(
    ProtocolModel(..),
    TrafficModel(..),
    TrafficGenerator(..),
    loadModel,
    saveModel,
    generateLength
)
where

import GHC.Generics
import Data.Serialize (Serialize, encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Control.Monad.State.Lazy (State(..), get, put)
import Control.Lens
import Control.Lens.TH

import Dust.Crypto.PRNG
import Dust.Model.PacketLength
import qualified Dust.Model.Content as C
import Dust.Model.Port

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

data TrafficGenerator = TrafficGenerator {
    _genModel :: ProtocolModel,
    _genPrng :: PRNG
  }

makeLenses ''TrafficGenerator

loadModel :: FilePath -> IO (Either String ProtocolModel)
loadModel path = do
    s <- B.readFile path
    return ((decode s)::(Either String ProtocolModel))

saveModel :: ProtocolModel -> FilePath -> IO ()
saveModel model path = do
  let s = encode model
  B.writeFile path s

generateLength :: State TrafficGenerator Word16
generateLength = do
  gen <- get
  let prng = gen ^. genPrng
  let (result, prng') = randomWord16 prng
  genPrng .= prng'
  return result

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
