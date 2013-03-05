{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Services.Sneakermesh.Message
(
 Command(..),
 ResultMessage(..),
 MessageID(..),
 Message(..)
)
where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Data.ByteString

data Command =
      PutMessage Message
    | GetIndex
    | GetMessages [MessageID]
    deriving (Show, Read, Generic)

newtype MessageID = MessageID ByteString deriving (Show, Read, Generic)
type Message = ByteString

instance Serialize Command
instance Serialize MessageID

data ResultMessage =
      IndexResult [MessageID]
    | MessagesResult [Message]
    deriving (Show, Read, Generic)

instance Serialize ResultMessage
