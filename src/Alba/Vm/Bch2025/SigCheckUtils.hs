-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Alba.Vm.Bch2025.SigCheckUtils (checkSig, verifySignature) where

import Alba.Tx.Bch2025 (Hash256 (..))
import Alba.Vm.Bch2025.TxContext (TxContext)
import Alba.Vm.Bch2025.VmSigHash (signatureHash)
import Alba.Vm.Common.Crypto
  ( ecdsaVerify,
    importPubKey,
    importSig,
    schnorrVerify,
  )
import Alba.Vm.Common.Crypto qualified as CR
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.StackElement (Bytes)
import Data.ByteString qualified as B
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (seq)

checkSig :: TxContext -> CodeL1 -> Bytes -> Bytes -> Maybe (Bool, Int)
checkSig _txContext _code sig pubKey | B.null pubKey || B.null sig = Nothing
checkSig txContext code sig pubKey =
  let Just (sig', sigHashType) = B.unsnoc sig
      sigHashType' = fromIntegral sigHashType
      (Hash256 hash, imgSize) = signatureHash txContext code sigHashType'
   in Just (verifySignature sig' pubKey hash, imgSize)

verifySignature :: Bytes -> Bytes -> Bytes -> Bool
verifySignature sig _pubKey _hash | B.null sig = False
verifySignature sig pubKey hash
  | B.length sig == 64 =
      let Just pubKey' = unsafePerformIO $ importPubKey pubKey
          Just sig' = CR.sig sig
       in unsafePerformIO $ schnorrVerify pubKey' sig' hash
verifySignature sig pubKey hash =
  let Just pubKey' = unsafePerformIO $ importPubKey pubKey
      Just sig' = unsafePerformIO $ importSig sig
   in not (B.null sig) && unsafePerformIO (ecdsaVerify pubKey' sig' hash)
