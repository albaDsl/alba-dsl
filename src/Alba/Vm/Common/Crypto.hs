-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.Crypto
  ( CS.Sig (..),
    CS.PubKey (..),
    CS.SecKey (..),
    sign,
    signDer,
    ecdsaVerify,
    schnorrVerify,
    importSig,
    importPubKey,
    exportPubKey,
    sig,
  )
where

import Crypto.Secp256k1 (Ctx (..), Msg (..), PubKey (..), SecKey, Sig (..))
import Crypto.Secp256k1 qualified as CS
import Data.ByteString qualified as B
import Data.IORef (IORef, newIORef, readIORef)
import Secp256k1 qualified as S
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE initCtx #-}
initCtx :: IORef Ctx
initCtx = unsafePerformIO $ do
  res <- CS.createContext
  newIORef res

getCtx :: IO Ctx
getCtx = readIORef initCtx

{-# NOINLINE initCtx2 #-}
initCtx2 :: IORef S.Ctx
initCtx2 = unsafePerformIO $ do
  res <- S.contextCreate
  newIORef res

getCtx2 :: IO S.Ctx
getCtx2 = readIORef initCtx2

sign :: B.ByteString -> SecKey -> IO Sig
sign bytes sec = do
  ctx <- getCtx
  pure $ CS.signMsg ctx sec (Msg bytes)

signDer :: B.ByteString -> SecKey -> IO B.ByteString
signDer bytes sec = do
  ctx <- getCtx
  pure $ CS.exportSig ctx (CS.signMsg ctx sec (Msg bytes))

ecdsaVerify :: PubKey -> Sig -> B.ByteString -> IO Bool
ecdsaVerify (PubKey pub) (Sig sig') bytes = do
  ctx <- getCtx2
  pure $ S.ecdsaVerify ctx pub sig' bytes

schnorrVerify :: PubKey -> Sig -> B.ByteString -> IO Bool
schnorrVerify (PubKey pub) (Sig sig') bytes = do
  ctx <- getCtx2
  pure $ S.schnorrVerify ctx pub sig' bytes

importSig :: B.ByteString -> IO (Maybe Sig)
importSig sigDer = do
  ctx <- getCtx
  pure $ CS.importSig ctx sigDer

importPubKey :: B.ByteString -> IO (Maybe PubKey)
importPubKey pubKeyDer = do
  ctx <- getCtx
  pure $ CS.importPubKey ctx pubKeyDer

exportPubKey :: Bool -> PubKey -> IO B.ByteString
exportPubKey compressed pubKey = do
  ctx <- getCtx
  pure $ CS.exportPubKey ctx compressed pubKey

sig :: B.ByteString -> Maybe Sig
sig = CS.sig
