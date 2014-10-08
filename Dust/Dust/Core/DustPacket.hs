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
import Dust.Crypto.Cipher
import Data.ByteString
import qualified Data.ByteString as B
import Data.Serialize
import Data.Int
import Debug.Trace

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

decryptHeader :: Cipher -> CipherHeader -> PlainHeader
decryptHeader cipher (CipherHeader ciphertext) =
    let (Plaintext lengthBytes, cipher') = decrypt cipher ciphertext
    in decodeHeader lengthBytes

decodeHeader :: ByteString -> PlainHeader
decodeHeader bs =
    case (decode bs)::(Either String Int32) of
        Left error -> PlainHeader 0
        Right value -> PlainHeader value

encodeHeader :: PlainHeader -> ByteString
encodeHeader (PlainHeader i) = encode i

encryptData :: Cipher -> PlainDataPacket -> CipherDataPacket
encryptData cipher (PlainDataPacket header@(PlainHeader i) plaintext) =
    let plainheader = Plaintext (encodeHeader header)
        (cipherheader, cipher') = encrypt cipher plainheader
        (ciphertext, cipher'') = encrypt cipher' plaintext
    in CipherDataPacket (CipherHeader cipherheader) ciphertext

decryptData :: Cipher-> CipherDataPacket -> PlainDataPacket
decryptData cipher (CipherDataPacket header ciphertext) =
    let (plaintext@(Plaintext bs), cipher') = decrypt cipher ciphertext
    in PlainDataPacket (PlainHeader (length32 bs)) plaintext

length32 :: ByteString -> Int32
length32 bs = (fromIntegral (B.length bs)) :: Int32
