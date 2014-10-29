module Dust.Control.Command
(
  processCommand
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Binary (encode)
import Control.Monad.State.Lazy
import Control.Lens
import Debug.Trace

import Dust.Model.TrafficModel
import Dust.Crypto.PRNG
import Dust.Control.Protocol
import Dust.Control.State

processCommand :: ControlRequest -> State TrafficGenerator (Maybe BL.ByteString)
processCommand Duration = do
  gen <- get
  let (result, gen') = runState generateDuration gen
  put gen'
  trace ("duration -> " ++ show result) $ return $ Just $ encode result
processCommand (PacketCount ms) = do
  gen <- get
  let (result, gen') = runState (generatePacketCount ms) gen
  put gen'
  trace ("count " ++ show ms ++ " -> " ++ show result) $ return $ Just $ encode result
processCommand EncodedReady = do
  gen <- get
  pad <- use $ config.padding
  if pad
    then do
      let (result, gen') = runState generateLength gen
      put gen'
      trace ("encoded ready -> " ++ show result) $ return $ Just $ encode result
    else do
      let (result, gen') = runState encodedReady gen
      put gen'
      trace ("encoded ready -> " ++ show result) $ return $ Just $ encode result
processCommand (DecodedReady) = do
  gen <- get
  let (result, gen') = runState decodedReady gen
  put gen'
  trace ("decoded ready -> " ++ show result) $ return $ Just $ encode result
processCommand (PutEncoded bs) = do
  gen <- get
  let (_, gen') = runState (putEncoded bs) gen
  put gen'
  trace ("put encoded " ++ (show $ B.length bs)) $ return Nothing
processCommand (PutDecoded bs) = do
  gen <- get
  let (_, gen') = runState (putDecoded bs) gen
  put gen'
  trace ("put decoded " ++ (show $ B.length bs)) $ return Nothing
processCommand (GetEncoded len) = do
  gen <- get
  let ilen = fromIntegral len
  pad <- use $ config.padding
  let (maybeResult, gen') = runState (getEncoded len) gen
  traceShow maybeResult $ put gen'
  case maybeResult of
    Nothing -> do
      if pad
        then do
          let (padding, gen'') = runState (generatePadding ilen) gen
          traceShow padding $ put gen''
          return $ Just (padding ^. from strict)
        else return Nothing
    Just result -> do
      let blen = B.length result
      let remaining = ilen-blen
      if pad && remaining>0
        then do
          let (padding, gen'') = runState (generatePadding remaining) gen
          traceShow padding $ put gen''
          return $ Just $ BL.append (encode result) (padding ^. from strict)
        else trace ("get encoded " ++ show len ++ " -> " ++ (show $ B.length result)) $ return $ Just $ encode result
processCommand (GetDecoded len) = do
  gen <- get
  let (maybeResult, gen') = runState (getDecoded len) gen
  traceShow maybeResult $ put gen'
  case maybeResult of
    Nothing -> trace "Get decoded returned Nothing" $ return Nothing
    Just result -> trace ("get decoded " ++ show len ++ " -> " ++ (show $ B.length result)) $ return $ Just $ encode result
