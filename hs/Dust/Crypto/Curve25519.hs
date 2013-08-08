{-# CFILES lib/curve25519-donna.c #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dust.Crypto.Curve25519 (curve25519) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCString)

import Foreign.Marshal.Unsafe (unsafeLocalState)

curve25519 :: ByteString -> ByteString -> ByteString
curve25519 bs1 bs2 = curve
	where
	Just curve = unsafeLocalState $ unsafe_curve25519 bs1 bs2

unsafe_curve25519 :: ByteString -> ByteString -> IO (Maybe ByteString)
unsafe_curve25519 secret basepoint
    | B.length secret >= 32 && B.length basepoint >= 32 =
        -- This ByteString will be overwritten by the C call, but so long as
        -- the C side does not keep a reference to it afterward, it does what we
        -- expect and saves us a copy
        unsafeUseAsCString outBS $ \output ->
            B.useAsCString input1 $ \csecret ->
                B.useAsCString input2 $ \cbasepoint -> do
                    result <- c_curve25519_donna output csecret cbasepoint
                    case result of
                        0 -> return outBS
                        _ -> return Nothing
    | otherwise = return Nothing
    where
    outBS = B.replicate 32 0xAB

-- Should be Ptr Word8 (not CString), but this should be safe if we just
-- use them as bytes

foreign import ccall unsafe "curve25519_donna" c_curve25519_donna ::
    CString -> CString -> CString -> IO Int
