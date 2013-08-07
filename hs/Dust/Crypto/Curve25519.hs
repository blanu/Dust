{-# CFILES lib/curve25519-donna.c #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dust.Crypto.Curve25519
(
  c_curve25519_donna,
  unsafe_curve25519,
  curve25519
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Data.Word
import Foreign.Marshal.Alloc (mallocBytes)

withByteStringPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f =
    withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
    where (fptr, off, _) = toForeignPtr b

curve25519 :: ByteString -> ByteString -> ByteString
curve25519 bs1 bs2 = unsafePerformIO $ unsafe_curve25519 bs1 bs2

unsafe_curve25519 :: ByteString -> ByteString -> IO ByteString
unsafe_curve25519 input1 input2 = do
    output <- mallocBytes 32
    withByteStringPtr input1 $ \cinput1 -> do
        withByteStringPtr input2 $ \cinput2 -> do
            result <- c_curve25519_donna output cinput1 cinput2
            unsafePackCStringFinalizer output 32 emptyFinalizer

emptyFinalizer :: IO()
emptyFinalizer = return ()

foreign import ccall unsafe "curve25519_donna" c_curve25519_donna :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Int)
