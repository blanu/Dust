{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.SignedDustPacket
(
 DustMessage(..),
 makeSignedPlainPacket,
 verifySignedPlainPacket,
 makeEncodedMessage,
 verifyEncodedMessage
) where

import GHC.Generics
import Dust.Crypto.Cipher
import Data.ByteString
import qualified Data.ByteString as B
import Data.Serialize
import Data.Int

import Dust.Core.DustPacket
import Dust.Crypto.Keys
import Dust.Crypto.ECDSA

data DustMessage = DustMessage Bool Plaintext (Maybe (PublicKey, Signature)) deriving (Show, Eq, Generic) -- ignore payload [signature]
instance Serialize DustMessage

makeEncodedMessage :: Bool -> Plaintext -> (Maybe Keypair) -> ByteString
makeEncodedMessage ignore payload maybeKeys = do
    let msg = makeMessage ignore payload maybeKeys
    encode msg

makeMessage :: Bool -> Plaintext -> (Maybe Keypair) -> DustMessage
makeMessage ignore payload Nothing = DustMessage ignore payload Nothing
makeMessage True payload _ = DustMessage True payload Nothing
makeMessage False payload@(Plaintext bs) (Just keypair@(Keypair public private)) = DustMessage False payload (Just (public, sign bs private))

makeSignedPlainPacket :: Plaintext -> Keypair -> PlainDataPacket
makeSignedPlainPacket plain keypair = do
    let msg = makeMessage False plain $ Just keypair
    let bs' = encode msg
    makePlainPacket (Plaintext bs')

verifySignedPlainPacket :: PlainDataPacket -> Maybe (PublicKey, Plaintext)
verifySignedPlainPacket (PlainDataPacket header (Plaintext plaintext)) = verifyEncodedMessage plaintext

verifyEncodedMessage :: ByteString -> Maybe (PublicKey, Plaintext)
verifyEncodedMessage plaintext = do
    let eitherMsg = (decode plaintext) :: (Either String DustMessage)
    case eitherMsg of
        Left error -> Nothing
        Right (DustMessage ignore plain@(Plaintext payload) maybeSig) -> do
            case ignore of
                True -> Nothing
                False -> do
                    case maybeSig of
                        Nothing -> Nothing
                        Just (public, sig) -> do
                            case verify payload sig public of
                                False -> Nothing
                                True  -> Just (public, plain)
