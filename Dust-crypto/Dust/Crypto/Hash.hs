module Dust.Crypto.Hash
(
 digest
)
where

import Data.ByteString
import Data.Serialize
import Crypto.Classes

import Crypto.Skein

digest :: ByteString -> ByteString
digest bs =
    let h = hash' bs :: Skein_256_256
    in encode h

mac :: ByteString -> Key -> ByteString
mac bs key =
    let m = skeinMAC' key bs :: Skein_256_256
    in encode m

