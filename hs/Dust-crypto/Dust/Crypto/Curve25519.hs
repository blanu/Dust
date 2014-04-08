{-# CFILES lib/curve25519-donna.c #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dust.Crypto.Curve25519 (curve25519) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Foreign.C.String (CString, CStringLen)

import Foreign.Marshal.Unsafe (unsafeLocalState)

curve25519 :: ByteString -> ByteString -> ByteString
curve25519 bs1 bs2 = curve
	where
	Just curve = unsafeLocalState $ unsafe_curve25519 bs1 bs2

--unsafe_curve25519 :: ByteString -> ByteString -> IO (Maybe ByteString)
--unsafe_curve25519 secret basepoint
--    | B.length secret >= 32 && B.length basepoint >= 32 =
--        -- This ByteString will be overwritten by the C call, but so long as
--        -- the C side does not keep a reference to it afterward, it does what we
--        -- expect and saves us a copy
----        unsafeUseAsCString outBS $ \output ->
--        B.useAsCString secret $ \csecret ->
--            B.useAsCString basepoint $ \cbasepoint -> do
--                outs@(output, _) <- newCAStringLen outBS
--                result <- c_curve25519_donna output csecret cbasepoint
--                case result of
--                    0 -> do
--                        value <- peekCStringLen outs
--                        return $ Just $ B8.pack value
--                    _ -> return Nothing
--    | otherwise = return Nothing
--    where
----    outBS = B.replicate 32 0xAB
--    outBS = replicate 32 '\171'


-- Should be Ptr Word8 (not CString), but this should be safe if we just
-- use them as bytes

--foreign import ccall unsafe "curve25519_donna" c_curve25519_donna ::
--    CString -> CString -> CString -> IO Int

unsafe_curve25519 :: ByteString -> ByteString -> IO (Maybe ByteString)
unsafe_curve25519 secret basepoint
    | B.length secret >= 32 && B.length basepoint >= 32 =
        -- This ByteString will be overwritten by the C call, but so long as
        -- the C side does not keep a reference to it afterward, it does what we
        -- expect and saves us a copy
        B.useAsCStringLen secret $ \(csecret, _) ->
            B.useAsCStringLen basepoint $ \(cbasepoint, _) -> do
                result <- c_curve25519_safe csecret cbasepoint
                let resultLen = (result, 32)
                outBS <- unsafePackCStringLen resultLen
                return $ Just outBS
    | otherwise = return Nothing

-- Should be Ptr Word8 (not CString), but this should be safe if we just
-- use them as bytes

foreign import ccall unsafe "curve25519_safe" c_curve25519_safe ::
    CString -> CString -> IO CString
