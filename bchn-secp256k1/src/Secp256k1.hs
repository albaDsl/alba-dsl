-- Copyright (c) 2025 albaDsl

module Secp256k1 where

import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as B
import Foreign (Ptr)
import Foreign.C (CChar (..), CInt (..))
import System.IO.Unsafe (unsafePerformIO)

type Ctx = Ptr CChar

foreign import ccall safe "secp256k1_context_create"
  cContextCreate :: CInt -> IO (Ptr CChar)

foreign import ccall safe "secp256k1_ecdsa_verify"
  cEcdsaVerify :: Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt

foreign import ccall safe "secp256k1_schnorr_verify"
  cSchnorrVerify :: Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt

contextCreate :: IO Ctx
contextCreate = cContextCreate 0x0301

ecdsaVerify :: Ptr CChar -> B.ByteString -> B.ByteString -> B.ByteString -> Bool
ecdsaVerify = verify cEcdsaVerify

schnorrVerify ::
  Ptr CChar ->
  B.ByteString ->
  B.ByteString ->
  B.ByteString ->
  Bool
schnorrVerify = verify cSchnorrVerify

verify ::
  (Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt) ->
  Ptr CChar ->
  B.ByteString ->
  B.ByteString ->
  B.ByteString ->
  Bool
verify f ctx pubKey sig msg =
  unsafePerformIO $ do
    pubKey' <- cStr pubKey
    sig' <- cStr sig
    msg' <- cStr msg
    toBool <$> f ctx sig' msg' pubKey'
  where
    cStr bs = B.unsafeUseAsCStringLen bs (\(ptr, _) -> pure ptr)

    toBool res =
      case res of
        0 -> False
        1 -> True
        _ -> error "Unexpected result."
