module Dust.Core.Invite
(
)
where

import Network
import Data.ByteString

import Dust.Crypto.Keys
import Dust.Crypto.Cipher
import Dust.Model.TrafficModel

data Invite = Invite {
    address :: Address,
    encryptionKey :: PublicKey,
    signingKey :: PublicKey,
    model :: TrafficModel,
    nonce :: ByteString
}

data Address = Address {
    v6 :: Bool,
    host :: String,
    port :: PortNumber
}
