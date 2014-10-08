{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Crypto.Keys
(
   PublicKey(..),
   PrivateKey(..),
   Keypair(..),

   loadKeypair,
   ensureKeys,
   saveKeypair,
   loadPublic,

   loadSigningKeypair,
   saveSigningKeypair
) where

import GHC.Generics
import Data.Serialize
import Data.ByteString
import System.IO.Error

import Dust.Crypto.PRNG

newtype PublicKey = PublicKey { publicBytes :: ByteString } deriving (Show, Eq, Generic)
newtype PrivateKey = PrivateKey { privateBytes :: ByteString } deriving (Show, Eq, Generic)

instance Serialize PublicKey

data Keypair = Keypair {
    public :: PublicKey,
    private :: PrivateKey
} deriving (Show, Eq)

ensureKeys :: String -> (IO Keypair) -> IO (Keypair, Bool)
ensureKeys keypath creator = do
    result <- tryIOError $ loadKeypair keypath
    case result of
        Right keypair -> return (keypair, False)
        Left _ -> do
            keys <- creator
            saveKeypair keypath keys
            return (keys, True)

loadKeypair :: String -> IO (Keypair)
loadKeypair keypath = do
    public <- loadPublic $ keypath ++ "/id.pub"
    private <- loadPrivate $ keypath ++ "/id.priv"
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

saveKeypair :: String -> Keypair -> IO ()
saveKeypair keypath (Keypair public private) = do
    savePublic (keypath ++ "id.pub") public
    savePrivate (keypath ++ "id.priv") private

savePrivate :: FilePath -> PrivateKey -> IO ()
savePrivate path (PrivateKey bs) = do
    saveKey path bs

savePublic :: FilePath -> PublicKey -> IO ()
savePublic path (PublicKey bs) = do
    saveKey path bs

saveKey :: FilePath -> ByteString -> IO ()
saveKey path bs = Data.ByteString.writeFile path bs

loadSigningKeypair :: IO (Keypair)
loadSigningKeypair = do
    public <- loadPublic "sign.pub"
    private <- loadPrivate "sign.priv"
    return (Keypair public private)

saveSigningKeypair :: Keypair -> IO ()
saveSigningKeypair (Keypair public private) = do
    savePublic "sign.pub" public
    savePrivate "sign.priv" private
