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
import Data.ByteString.Unsafe (unsafeUseAsCString)

import Foreign.C
import Foreign.Marshal.Unsafe (unsafeLocalState)

ed25519_publickey :: ByteString -> ByteString
ed25519_publickey privkey = pubkey
    where
    Just pubkey = fmap unsafeLocalState (unsafe_ed25519_publickey privkey)

ed25519_sign_open :: ByteString -> ByteString -> ByteString -> Bool
ed25519_sign_open msg pubkey signature = result
	where
	Just result = fmap unsafeLocalState $ unsafe_ed25519_sign_open msg pubkey signature

ed25519_sign :: ByteString -> ByteString -> ByteString -> ByteString
ed25519_sign msg secret pubkey = signature
	where
	Just signature = fmap unsafeLocalState $ unsafe_ed25519_sign msg secret pubkey

unsafe_ed25519_publickey :: ByteString -> Maybe (IO ByteString)
unsafe_ed25519_publickey input
    | B.length input <= 32 = Just $
        -- This ByteString will be overwritten by the C call, but so long as
        -- the C side does not keep a reference to it afterward, it does what we
        -- expect and saves us a copy
        unsafeUseAsCString outBS $ \output ->
            B.useAsCString input $ \cinput -> do
                c_ed25519_publickey cinput output
                return outBS
    | otherwise = Nothing
    where
    outBS = B.replicate 32 0xAB
    {-# NOINLINE outBS #-}

unsafe_ed25519_sign_open :: ByteString -> ByteString -> ByteString -> Maybe (IO Bool)
unsafe_ed25519_sign_open msg pubkey signature
    | B.length pubkey <= 32 && B.length signature <= 64 = Just $
        B.useAsCStringLen msg $ \(cmsg, cmsglen) ->
            B.useAsCString pubkey $ \cpubkey ->
                B.useAsCString signature $ \csignature ->
                    let result = c_ed25519_sign_open cmsg (fromIntegral cmsglen) cpubkey csignature in
                        return (result == 0)
    | otherwise = Nothing

unsafe_ed25519_sign :: ByteString -> ByteString -> ByteString -> Maybe (IO ByteString)
unsafe_ed25519_sign msg secret pubkey
    | B.length secret <= 32 && B.length pubkey <= 32 = Just $
        -- This ByteString will be overwritten by the C call, but so long as
        -- the C side does not keep a reference to it afterward, it does what we
        -- expect and saves us a copy
        unsafeUseAsCString outBS $ \signature ->
            B.useAsCStringLen msg $ \(cmsg, cmsglen) ->
                B.useAsCString secret $ \csecret ->
                    B.useAsCString pubkey $ \cpubkey -> do
                        c_ed25519_sign cmsg (fromIntegral cmsglen) csecret cpubkey signature
                        return outBS
    | otherwise = Nothing
    where
    outBS = B.replicate 64 0xAB
    {-# NOINLINE outBS #-}

-- Should be Ptr CUChar (not CString), but this should be safe if we just
-- use them as bytes

foreign import ccall unsafe "ed25519_publickey" c_ed25519_publickey ::
   CString -> CString -> IO ()

foreign import ccall unsafe "ed25519_sign_open" c_ed25519_sign_open ::
    CString -> CSize -> CString -> CString -> CInt

foreign import ccall unsafe "ed25519_sign" c_ed25519_sign ::
    CString -> CSize -> CString -> CString -> CString -> IO ()
