module Dust.Crypto.Curve
(
  splitSecret,
  createPrivate,
  createPublic,
  createKeypair,
  createShared,
  createEphemeral
) where

import Data.ByteString as B
import Data.Word
import Data.Bits
import System.Entropy

import Dust.Crypto.Keys
import Dust.Crypto.DustCipher
import Dust.Crypto.Curve25519

createEphemeral :: IO (Keypair)
createEphemeral = do
    entropy <- getEntropy 32
    return (createKeypair entropy)

splitSecret :: ByteString -> (Word8,ByteString,Word8)
splitSecret bs = let firstByte = B.head bs
                     lastByte  = B.last bs
                     middle = B.tail (B.init bs)
                 in (firstByte,middle,lastByte)

createPrivate :: ByteString -> PrivateKey
createPrivate bs = let (firstByte,middle,lastByte) = splitSecret bs
                       firstByte' = firstByte .&. 248
                       lastByte'  = (lastByte .&. 127) .|. 64
                   in PrivateKey (firstByte' `cons` middle `snoc` lastByte')

createPublic :: PrivateKey -> PublicKey
createPublic private = let bps = pack [9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                       in PublicKey (curve25519 (privateBytes private) bps)

createKeypair :: ByteString -> Keypair
createKeypair entropy = let private = createPrivate entropy
                            public = createPublic private
                        in Keypair public private

createShared :: PrivateKey -> PublicKey -> EncryptionKey
createShared private public = EncryptionKey (curve25519 (privateBytes private) (publicBytes public))
