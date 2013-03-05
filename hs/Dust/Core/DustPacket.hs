{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.DustPacket
(
 PlainHeader(..),
 CipherHeader(..),
 PlainDataPacket(..),
 CipherDataPacket(..),
 makePlainPacket,
 makeCipherPacket,
 encryptData,
 decryptData,
 decryptHeader,
 length32
) where

import GHC.Generics
import Dust.Crypto.DustCipher
import Data.ByteString
import qualified Data.ByteString as B
import Data.Serialize
import Data.Int

data PlainHeader = PlainHeader {
    payloadLength :: Int32
} deriving (Show, Eq, Generic)

data CipherHeader = CipherHeader {
    encryptedPayloadLength :: Ciphertext
} deriving (Show, Eq, Generic)

data PlainDataPacket = PlainDataPacket PlainHeader Plaintext deriving (Show, Eq, Generic)
data CipherDataPacket = CipherDataPacket CipherHeader Ciphertext deriving (Show, Eq, Generic)

instance Serialize PlainHeader
instance Serialize CipherHeader
instance Serialize PlainDataPacket
instance Serialize CipherDataPacket

makePlainPacket :: Plaintext -> PlainDataPacket
makePlainPacket (Plaintext bs) = PlainDataPacket (PlainHeader (length32 bs)) (Plaintext bs)

makeCipherPacket :: Ciphertext -> Ciphertext -> CipherDataPacket
makeCipherPacket lengthCiphertext ciphertext = CipherDataPacket (CipherHeader lengthCiphertext) ciphertext

decryptHeader :: (Ciphertext -> Plaintext) -> CipherHeader -> PlainHeader
decryptHeader cipher (CipherHeader ciphertext) =
    let (Plaintext lengthBytes) = cipher ciphertext
        lengthValue = decodeHeader lengthBytes
    in PlainHeader lengthValue

decodeHeader :: ByteString -> Int32
decodeHeader bs =
    case (decode bs)::(Either String Int32) of
        Left _ -> 0
        Right value -> value

encryptData :: (Plaintext -> Ciphertext) -> PlainDataPacket -> CipherDataPacket
encryptData cipher (PlainDataPacket header plaintext) =
    let cipherheader = cipher (Plaintext (encode header))
        ciphertext = cipher plaintext
    in CipherDataPacket (CipherHeader cipherheader) ciphertext

decryptData :: (Ciphertext -> Plaintext) -> CipherDataPacket -> PlainDataPacket
decryptData cipher (CipherDataPacket header ciphertext) =
    let plaintext@(Plaintext bs) = cipher ciphertext
    in PlainDataPacket (PlainHeader (length32 bs)) plaintext

length32 :: ByteString -> Int32
length32 bs = (fromIntegral (B.length bs)) :: Int32
