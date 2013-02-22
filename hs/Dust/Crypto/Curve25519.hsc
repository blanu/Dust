{-# CFILES lib/curve25519-donna.c #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dust.Crypto.Curve25519
(
  c_curve25519_donna,
  unsafe_curve25519,
  curve25519
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, useAsCString, packCString)
import Data.ByteString.Internal

import Foreign.C (CString)
import System.IO.Unsafe (unsafePerformIO)

curve25519 :: ByteString -> ByteString -> ByteString
curve25519 bs1 bs2 = unsafePerformIO $ unsafe_curve25519 bs1 bs2

unsafe_curve25519 :: ByteString -> ByteString -> IO ByteString
unsafe_curve25519 input1 input2 = do
    useAsCString (pack "1234567890123457890123456789012") $ \output -> do
        useAsCString input1 $ \cinput1 -> do
            useAsCString input2 $ \cinput2 -> do
                result <- c_curve25519_donna output cinput1 cinput2
                packCString output

foreign import ccall unsafe "curve25519_donna" c_curve25519_donna :: CString -> CString -> CString -> IO (Int)
