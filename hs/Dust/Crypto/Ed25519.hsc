{-# CFILES lib/ed25519-donna/ed25519.c #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dust.Crypto.Ed25519
(
    ed25519_publickey,
    ed25519_sign_open,
    ed25519_sign
) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Data.Word

--type WordSize = Word64 --Set to Word32 or Word64 depending on 32-bit or 64-bit platform
type WordSize = Word32 --Set to Word32 or Word64 depending on 32-bit or 64-bit platform

withByteStringPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f =
    withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
    where (fptr, off, _) = toForeignPtr b

ed25519_publickey :: ByteString -> ByteString
ed25519_publickey bs = unsafePerformIO $ unsafe_ed25519_publickey bs

ed25519_sign_open :: ByteString -> ByteString -> ByteString -> Bool
ed25519_sign_open msg pubkey signature = unsafePerformIO $ unsafe_ed25519_sign_open msg pubkey signature

ed25519_sign :: ByteString -> ByteString -> ByteString -> ByteString
ed25519_sign msg secret pubkey = unsafePerformIO $ unsafe_ed25519_sign msg secret pubkey

unsafe_ed25519_publickey :: ByteString -> IO ByteString
unsafe_ed25519_publickey input = do
    withByteStringPtr (pack "1234567890123457890123456789012") $ \output -> do
        withByteStringPtr input $ \cinput -> do
            c_ed25519_publickey cinput output
            unsafePackCStringFinalizer output 32 emptyFinalizer

unsafe_ed25519_sign_open :: ByteString -> ByteString -> ByteString -> IO Bool
unsafe_ed25519_sign_open msg pubkey signature = do
    withByteStringPtr msg $ \cmsg -> do
        withByteStringPtr pubkey $ \cpubkey -> do
            withByteStringPtr signature $ \csignature -> do
                let msgSize = CSize ((fromIntegral $ B.length msg)::WordSize)
                result <- c_ed25519_sign_open cmsg msgSize cpubkey csignature
                case result of
                    0 -> return (True)
                    otherwise -> return (False)

unsafe_ed25519_sign :: ByteString -> ByteString -> ByteString -> IO ByteString
unsafe_ed25519_sign msg secret pubkey = do
    withByteStringPtr (pack "12345678901234578901234567890123456789012345789012345678901234") $ \signature -> do
        withByteStringPtr msg $ \cmsg -> do
            withByteStringPtr secret $ \csecret -> do
                withByteStringPtr pubkey $ \cpubkey -> do
                    let msgSize = CSize ((fromIntegral $ B.length msg)::WordSize)
                    c_ed25519_sign cmsg msgSize csecret cpubkey signature
                    unsafePackCStringFinalizer signature 64 emptyFinalizer

emptyFinalizer :: IO()
emptyFinalizer = return ()

foreign import ccall unsafe "ed25519_publickey" c_ed25519_publickey :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "ed25519_sign_open" c_ed25519_sign_open :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> IO (Int)
foreign import ccall unsafe "ed25519_sign" c_ed25519_sign :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
