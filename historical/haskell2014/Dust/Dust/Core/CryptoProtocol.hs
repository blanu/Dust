{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Core.CryptoProtocol
(
 Session(..),
 Confirmation(..),
 Stream(..),
 StreamHeader(..),
 Handshake(..),
 makeSession,
 makeCipher,
 makeHeader,
 makeStream,
 makeEncoder,
 confirmSession,
 performEncryption,
 performDecryption
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Debug.Trace

import Dust.Core.DustPacket
import Dust.Crypto.Cipher
import Dust.Crypto.ECDH
import Dust.Crypto.Keys
import Dust.Crypto.Hash
import Dust.Crypto.Cipher

data Session = Session Keypair PublicKey IV Confirmation deriving (Show, Eq)
data Confirmation = Confirmation Ciphertext deriving (Show, Eq)
data Stream = Stream StreamHeader CipherDataPacket deriving (Show)
data StreamHeader = StreamHeader PublicKey IV deriving (Show)

data Handshake =
    -- Nothing has been sent or received
    Begin {_beginKeypair :: Keypair}
    -- Public key has been sent, awaiting public key from other side
  | SentKey {_sentKeypair :: Keypair}
    -- Public keys exchanged and confirmation code sent awaiting confirmation code from other side
  | SentConfirmation {_encryptionKey :: EncryptionKey, _confirmation :: B.ByteString}
    -- Confirmations exchanged and verified, ciphers created
  | Complete {_incomingCipher :: Cipher, _outgoingCipher :: Cipher}
    -- Confirmation code verificaiton failed
  | Failed

performEncryption :: B.ByteString -> Handshake -> (B.ByteString, B.ByteString, Handshake)
performEncryption plaintext (Begin keypair@(Keypair myPub@(PublicKey myPubBytes) myPriv)) = do
  -- Begin - nothing has been sent yet
  -- So check to see if the bytes we have is the public key for the other side
  -- Regardless, send our public key
  let plen = B.length plaintext
  if plen >= 32
    -- We have received the public key for the other side
    then do
      -- Extract the public key, leaving the remaining bytes
      let (otherPubBytes, rest) = B.splitAt 32 plaintext
      let otherPub = PublicKey otherPubBytes
      -- Complete the ECDH exchange to generate the encryption key
      let key = createShared myPriv otherPub
      -- Make confirmation code
      let myConf = makeConfirmationCode myPub otherPub
      -- Advance the handshake state
      let shake = SentConfirmation key myConf
      -- Send our public key and confirmation code, remember the rest, and the new handshake state
      let toSend = B.append myPubBytes myConf
      -- There might be more data left in the buffer, so let's try to process the next stage
      let (buff, rest', shake') = performEncryption rest shake
      (B.append toSend buff, rest', shake')
    -- We have not received the public key for the other side
    else do
      -- We still advance the handshake state so that we don't send our public key multiple times
      let shake = SentKey keypair
      -- Send our public key, the unused plaintext for next time, and the new handshake state
      (myPubBytes, plaintext, shake)
performEncryption plaintext shake@(SentKey keypair@(Keypair myPub myPriv)) = do
  -- SentKey - key has been sent, waiting for key from other side
  -- So check to see if the bytes we have is the public key for the other side
  let plen = B.length plaintext
  if plen >= 32
    -- We have received the public key for the other side
    then do
      -- Extract the public key, leaving the remaining bytes
      let (otherPubBytes, rest) = B.splitAt 32 plaintext
      let otherPub = PublicKey otherPubBytes
      -- Complete the ECDH exchange to generate the encryption key
      let key = createShared myPriv otherPub
      -- Make confirmation code
      let myConf = makeConfirmationCode myPub otherPub
      -- Advance the handshake state
      let shake = SentConfirmation key myConf
      -- There might be more data left in the buffer, so let's try to process the next stage
      let (buff, rest', shake') = performEncryption rest shake
      -- Send our confirmation code, remember the rest, and the new handshake state
      (B.append myConf buff, rest', shake')
    -- We have not received the public key for the other side
    else do
      -- Nothing to do but wait for the public key from the other side
      -- Send nothing, remember the unused plaintext for next time, and keep the same handshake state
      (B.empty, plaintext, shake)
performEncryption plaintext shake@(SentConfirmation key myConf) = do
  -- SentKey - key has been sent, waiting for key from other side
  -- So check to see if the bytes we have is the public key for the other side
  let plen = B.length plaintext
  if plen >= 32
    -- We have received the conirmation code for the other side
    then do
      -- Extract the confirmation code, leaving the remaining bytes
      let (otherConf, rest) = B.splitAt 32 plaintext
      -- Verify the confirmation code
      if otherConf == myConf
        then do
          -- Handshake is complete
          -- Derive incoming cipher
          let icipher = newCipherWithKey key
          -- Derive outgoing cipher
          let ocipher = newCipherWithKey key
          -- Set new handshake state
          let shake' = Complete icipher ocipher
          -- There might be more data left in the buffer, so let's try to process the next stage
          performEncryption rest shake'
        else do
          -- Send nothing, forget everything, set the handshake state to Failed
          (B.empty, B.empty, Failed)
    else do
      -- Nothing to do but wait for the confirmation code
      (B.empty, plaintext, shake)
-- If the handshake failed, we just discard everything forever
performEncryption _ Failed = (B.empty, B.empty, Failed)

performDecryption :: B.ByteString -> Handshake -> (B.ByteString, B.ByteString, Handshake)
performDecryption ciphertext shake = performEncryption ciphertext shake

makeConfirmationCode :: PublicKey -> PublicKey -> B.ByteString
makeConfirmationCode (PublicKey bs1) (PublicKey bs2) = do
  -- FIXME - This will not work. We need to differantiate the client and server public keys so we can maintain the ordering
  let input = B.append bs1 bs2
  digest input

makeSession :: Keypair -> PublicKey -> IV -> Session
makeSession keypair publicKey iv =
  let (conf, enc') = makeConfirmation keypair publicKey iv
  in Session keypair publicKey iv conf

confirmSession :: Session -> Bool
confirmSession session@(Session keypair publicKey iv _) =
    let session' = makeSession keypair publicKey iv
    in session == session'

makeConfirmation :: Keypair -> PublicKey -> IV -> (Confirmation, Cipher)
makeConfirmation keypair@(Keypair myPublic@(PublicKey pubbs) myPrivate) otherPublic@(PublicKey otherbs) iv@(IV ivbs) =
    let (pubA, pubB) = sortBytes pubbs otherbs
        plainConf = (pubA `B.append` pubB) `B.append` ivbs
        h = digest plainConf
        temp = Session keypair otherPublic iv (Confirmation $ Ciphertext B.empty)
        cipher = makeCipher temp
        (conf, cipher') = encrypt cipher $ Plaintext h
    in (Confirmation conf, cipher')

sortBytes :: ByteString -> ByteString -> (ByteString, ByteString)
sortBytes a b =
    if a < b
        then (a, b)
        else (b, a)

makeCipher :: Session -> Cipher
makeCipher (Session keypair@(Keypair myPublic myPrivate) otherPublic iv _) =
    let key = createShared myPrivate otherPublic
    in  newCipherIV key iv

makeHeader :: PublicKey -> IV -> StreamHeader
makeHeader publicKey iv = StreamHeader publicKey iv

makeStream :: StreamHeader -> CipherDataPacket -> Stream
makeStream header cipherPacket = Stream header cipherPacket

makeEncoder :: Session -> (Plaintext -> Stream)
makeEncoder session@(Session (Keypair myPublic _) _ iv _) =
    let header = makeHeader myPublic iv
        cipher = makeCipher session
        encrypter = encryptData cipher
        stream = makeStream header
    in stream . encrypter . makePlainPacket
