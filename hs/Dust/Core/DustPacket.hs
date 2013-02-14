module Dust.Core.DustPacket
(
    DustPacketHeader(..),
    CipherDataPacket(..),
    PlainDataPacket(..),

    encryptData,
    decryptData,
) where

import Dust.Crypto.DustCipher
import Data.ByteString

data DustPacketHeader = DustPacketHeader {
    iv :: IV
} deriving (Show, Eq)

data PlainDataPacket = PlainDataPacket DustPacketHeader Plaintext deriving (Show, Eq)
data CipherDataPacket = CipherDataPacket DustPacketHeader Ciphertext deriving (Show, Eq)

encryptData :: Key -> PlainDataPacket -> CipherDataPacket
encryptData key (PlainDataPacket header plaintext) = let packetIV = iv header
                                                         ciphertext = encrypt key packetIV plaintext
                                                     in CipherDataPacket (DustPacketHeader packetIV) ciphertext

decryptData :: Key -> CipherDataPacket -> PlainDataPacket
decryptData key (CipherDataPacket header ciphertext) = let packetIV = iv header
                                                           plaintext = decrypt key packetIV ciphertext
                                                       in PlainDataPacket (DustPacketHeader packetIV) plaintext
