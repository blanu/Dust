{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.Keys
(
   PublicKey(..),
   PrivateKey(..),
   Keypair(..),

   loadKeypair,
   saveKeypair,
   loadPublic,
) where

import GHC.Generics
import Data.Serialize
import Data.ByteString

newtype PublicKey = PublicKey { publicBytes :: ByteString } deriving (Show, Eq, Generic)
newtype PrivateKey = PrivateKey { privateBytes :: ByteString } deriving (Show, Eq, Generic)

instance Serialize PublicKey

data Keypair = Keypair {
    public :: PublicKey,
    private :: PrivateKey
} deriving (Show, Eq)

loadKeypair :: IO (Keypair)
loadKeypair = do
    public <- loadPublic "id.pub"
    private <- loadPrivate "id.priv"
    return (Keypair public private)

loadPrivate :: FilePath -> IO (PrivateKey)
loadPrivate path = do
    key <- loadKey path
    return (PrivateKey key)

loadPublic :: FilePath -> IO (PublicKey)
loadPublic path = do
    key <- loadKey path
    return (PublicKey key)

loadKey :: FilePath -> IO (ByteString)
loadKey path = Data.ByteString.readFile path


saveKeypair :: Keypair -> IO ()
saveKeypair (Keypair public private) = do
    savePublic "id.pub" public
    savePrivate "id.priv" private

savePrivate :: FilePath -> PrivateKey -> IO ()
savePrivate path (PrivateKey bs) = do
    saveKey path bs

savePublic :: FilePath -> PublicKey -> IO ()
savePublic path (PublicKey bs) = do
    saveKey path bs

saveKey :: FilePath -> ByteString -> IO ()
saveKey path bs = Data.ByteString.writeFile path bs
