{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Model.Observations
(
    Observations(..),
    LengthObservations(..),
    ContentObservations(..),
    SubstringObservations(..),
    emptyObservations,
    loadObservations,
    saveObservations,
    ensureObservations,
    observePacket,
    observePort,
    observeSubstrings,
    makeModel
)
where

import GHC.Generics
import Data.Serialize
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString as B
import Data.Int
import Data.Word
import System.Directory
import Data.List (nub)
import Data.Map (Map(..), alter, empty)

import Dust.Model.PacketLength
import qualified Dust.Model.Content as C
import Dust.Model.Port
import Dust.Model.TrafficModel

data Observations = Observations {
    lengths :: LengthObservations,
    content :: ContentObservations,
    ports   :: PortObservations,
    strings :: SubstringObservations
} deriving (Generic, Show)
instance Serialize Observations

data LengthObservations = LengthObservations [Int] deriving (Generic, Show)
instance Serialize LengthObservations

data ContentObservations = ContentObservations [Int] deriving (Generic, Show)
instance Serialize ContentObservations

data PortObservations = PortObservations [Int] deriving (Generic, Show)
instance Serialize PortObservations

data SubstringObservations = SubstringObservations [(Map ByteString Int)] deriving (Generic, Show)
instance Serialize SubstringObservations

observePacket :: Observations -> ByteString -> Observations
observePacket (Observations lobs cobs pobs sobs) bs =
  let bsl = (fromIntegral $ B.length bs)::Int
      lobs' = observeLengths lobs (bsl, 1)
      cobs' = observeByteString cobs bs
  in Observations lobs' cobs' pobs sobs

observePort :: Observations -> Int -> Observations
observePort (Observations lobs cobs (PortObservations ports) sobs) port =
  let pobs' = PortObservations $ nub $ port : ports
  in Observations lobs cobs pobs' sobs

observeSubstrings :: Observations -> ByteString -> Observations
observeSubstrings obs bs =
  let windows = windowed 17 bs  
  in observeSubstringList obs 0 windows

observeSubstringList :: Observations -> Int -> [ByteString] -> Observations
observeSubstringList obs offset [] = obs
observeSubstringList obs offset (bs:bss) = observeSubstringList (observeSubstring obs offset bs) (offset+1) bss

observeSubstring :: Observations -> Int -> ByteString -> Observations
observeSubstring (Observations lobs cobs pobs (SubstringObservations subs)) offset bs =
  let sobs' = SubstringObservations $ updateSubstringCount subs offset bs
  in Observations lobs cobs pobs sobs'  

updateSubstringCount :: [(Map ByteString Int)] -> Int -> ByteString -> [(Map ByteString Int)]
updateSubstringCount items index bs =
    let (a, rest) = splitAt index items
    in case rest of
      [] -> items
      (item:b) -> a ++ ((alter updateMapCount bs item):b)  

updateMapCount :: Maybe Int -> Maybe Int
updateMapCount Nothing = Just 1
updateMapCount (Just x) = Just (x+1)

windowed :: Int -> ByteString -> [ByteString]
windowed size ls = 
  if B.null ls
  then []
  else
    if B.length ls < size
      then [ls]
      else (B.take size ls) : (windowed size $ B.tail ls)

observeLengths :: LengthObservations -> (Int, Int) -> LengthObservations
observeLengths (LengthObservations items) item = LengthObservations $ updateCounts items item

updateCounts :: [Int] -> (Int, Int) -> [Int]
updateCounts items (index,count) = 
    let (a, (item:b)) = splitAt index items
    in  a ++ ((item+count):b)

observeByteString :: ContentObservations -> ByteString -> ContentObservations
observeByteString obs bs = observeBytes obs $ unpack bs

observeBytes :: ContentObservations -> [Word8] -> ContentObservations
observeBytes obs [] = obs
observeBytes obs (b:rest) =
  let bint = (fromIntegral b)::Int
      newObs = observeByte obs (bint,1)
  in  observeBytes newObs rest

observeByte :: ContentObservations -> (Int, Int) -> ContentObservations
observeByte (ContentObservations items) item = ContentObservations $ updateCounts items item

emptyObservations :: Observations
emptyObservations = Observations emptyLengthObservations emptyContentObservations emptyPortObservations emptySubstringObservations

emptyLengthObservations :: LengthObservations
emptyLengthObservations = LengthObservations (take 1520 $ repeat 0)

emptyContentObservations :: ContentObservations
emptyContentObservations = ContentObservations (take 256 $ repeat 0)

emptyPortObservations :: PortObservations
emptyPortObservations = PortObservations []

emptySubstringObservations :: SubstringObservations
emptySubstringObservations = SubstringObservations $ take 1500 $ repeat empty

loadObservations :: FilePath -> IO (Either String Observations)
loadObservations path = do
    s <- B.readFile path
    return ((decode s)::(Either String Observations))

saveObservations :: FilePath -> Observations -> IO()
saveObservations path obs = do
    putStrLn "Saving observations..."
    let s = encode obs
    putStrLn $ "Writing " ++ (show $ B.length s) ++ " bytes"
    B.writeFile path s
    putStrLn "Done."

ensureObservations :: FilePath -> IO Observations
ensureObservations path = do
    exists <- doesFileExist path
    case exists of
      True -> do
        eitherObs <- loadObservations path
        case eitherObs of
            Right obs -> return obs
            Left _ -> return emptyObservations
      False -> return emptyObservations

makeModel :: Observations -> TrafficModel
makeModel (Observations lengthObs contentObs portObs stringObs) =
    let lengthModel = makeLengthModel lengthObs
        contentModel = makeContentModel contentObs
        portModel = makePortModel portObs
    in TrafficModel lengthModel contentModel portModel

makeLengthModel :: LengthObservations -> PacketLengthModel
makeLengthModel (LengthObservations counts) =
  let total = sum counts
      probs = map (divideBy total) counts
  in PacketLengthModel probs

makeContentModel :: ContentObservations -> C.ContentModel
makeContentModel (ContentObservations obs) =
  C.makeContentModel $ zip ([0..255]::[Word8]) obs

makePortModel :: PortObservations -> PortModel
makePortModel (PortObservations ports) = PortModel ports

divideBy :: Int -> Int -> Double
divideBy d n = 
    let fd = (fromIntegral d)::Double
        fn = (fromIntegral n)::Double
    in fn / fd
