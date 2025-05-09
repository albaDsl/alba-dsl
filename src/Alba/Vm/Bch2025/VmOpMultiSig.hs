-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Alba.Vm.Bch2025.VmOpMultiSig (evalOpMultiSig) where

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Bch2025.SigCheckUtils (checkSig)
import Alba.Vm.Bch2025.SigEncoding
  ( checkPubKeyEncoding,
    checkRawEcdsaSignatureEncoding,
    checkTransactionSchnorrSignatureEncoding,
  )
import Alba.Vm.Bch2025.TxContext (TxContext)
import Alba.Vm.Bch2025.Utils (verifyMinStackSize)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement
  ( Bytes,
    StackElement (..),
    boolToStackElement,
    stackElementToBool,
    stackElementToBytes,
    stackElementToInteger,
  )
import Alba.Vm.Common.VmLimits (addBytesPushed, addHashIterations, addSigCheck)
import Alba.Vm.Common.VmState (VmState (..))
import Control.Monad (unless, when)
import Data.Bits (Bits (..), shift, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.Foldable (toList)
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as S
import Data.Word (Word64)
import Prelude hiding (seq)

schnorrSigSize :: Int
schnorrSigSize = 65

evalOpMultiSig ::
  OpcodeL2 ->
  CodeL1 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpMultiSig op _code txContext st@VmState {..} =
  case op of
    OP_CHECKMULTISIG -> Just $ opCheckMultiSig txContext st
    OP_CHECKMULTISIGVERIFY ->
      Just $ verifyOp opCheckMultiSig SeCheckMultiSigVerify
    _ -> Nothing
  where
    verifyOp f err = do
      st'@VmState {s = s' :|> res} <- f txContext st
      res' <- stackElementToBool params res
      if res' then Right $ st' {s = s'} else Left err

opCheckMultiSig :: TxContext -> VmState -> Either ScriptError VmState
opCheckMultiSig txContext st@VmState {..} = do
  (s1 :|> pubKeyCount) <- pure s
  pubKeyCount' <- fromIntegral <$> stackElementToInteger params pubKeyCount
  verifyPubKeyCountInRange pubKeyCount'
  verifyMinStackSize pubKeyCount' s1
  let (s2, pubKeys) = splitRight pubKeyCount' s1
  (s3 :|> sigCount) <- pure s2
  sigCount' <- fromIntegral <$> stackElementToInteger params sigCount
  verifySignatureCountInRange sigCount' pubKeyCount'
  verifyMinStackSize sigCount' s3
  let (s4, sigs) = splitRight sigCount' s3
  (s5 :|> bitfield) <- pure s4
  let sigs' = stackElementToBytes <$> toList sigs
      pubKeys' = stackElementToBytes <$> toList pubKeys
      bitfield' = stackElementToBytes bitfield
      st' = st {s = s5}
  if B.null bitfield'
    then do
      let sigs'' = reverse sigs'
          pubKeys'' = reverse pubKeys'
      st'' <- checkEcdsaMultiSig txContext sigs'' pubKeys'' signedCode st'
      Right $ addSigCheck (if all B.null sigs' then 0 else pubKeyCount') st''
    else do
      bitfield'' <- verifyBitfield bitfield' (length pubKeys) (length sigs)
      checkSchnorrMultiSig txContext bitfield'' sigs' pubKeys' signedCode st'
  where
    splitRight :: Int -> Seq a -> (Seq a, Seq a)
    splitRight n seq = S.splitAt (length seq - n) seq

    verifyPubKeyCountInRange :: Int -> Either ScriptError ()
    verifyPubKeyCountInRange pubKeyCount =
      unless (pubKeyCount >= 0 || pubKeyCount <= maxPubkeysPerMultisig) $
        Left SePubkeyCount

    maxPubkeysPerMultisig = 20 :: Int

    verifySignatureCountInRange sigCount pubKeyCount =
      unless (sigCount >= 0 || sigCount <= pubKeyCount) $ Left SeSigCount

verifyBitfield :: Bytes -> Int -> Int -> Either ScriptError Word64
verifyBitfield bitfield numKeys numSigs = do
  unless (numKeys <= 32) $ Left SeInvalidBitfieldSize
  unless
    (B.length bitfield == (numKeys + 7) `div` 8)
    $ Left SeInvalidBitfieldSize
  let bitfield' =
        B.foldr
          (\x a -> fromIntegral x .|. a `shift` 8)
          0
          bitfield ::
          Word64
  let mask = (1 `shift` numKeys) - 1 :: Word64
  unless ((bitfield' .&. mask) == bitfield') $ Left SeInvalidBitRange
  unless (popCount bitfield' == numSigs) $ Left SeInvalidBitCount
  Right bitfield'

checkSchnorrMultiSig ::
  TxContext ->
  Word64 ->
  [Bytes] ->
  [Bytes] ->
  CodeL1 ->
  VmState ->
  Either ScriptError VmState
checkSchnorrMultiSig _ bitfield sigs _ _ st
  | bitfield == 0 && null sigs =
      let res = boolToStackElement True
       in pure $ addBytesPushed res.byteSize $ st {s = st.s :|> res}
checkSchnorrMultiSig txContext bitfield (sig : sigs) (pubKey : pubKeys) code st
  | testBit bitfield 0 = do
      checkTransactionSchnorrSignatureEncoding sig
      checkPubKeyEncoding pubKey
      st' <- case checkSig txContext code sig pubKey of
        Just (res, imgSize) -> do
          unless res $ Left SeSigNullFail
          Right $ (addSigCheck 1 . addHashIterations imgSize True) st
        Nothing -> Left SeSigNullFail
      checkSchnorrMultiSig txContext (bitfield `shiftR` 1) sigs pubKeys code st'
checkSchnorrMultiSig txContext bitfield sigs (_ : pubKeys) code st
  | bitfield /= 0 =
      checkSchnorrMultiSig txContext (bitfield `shiftR` 1) sigs pubKeys code st
checkSchnorrMultiSig _ _ _ _ _ _ = canNotHappen

-- Sigs must be in the same order as corresponding pubkeys.
checkEcdsaMultiSig ::
  TxContext ->
  [Bytes] ->
  [Bytes] ->
  CodeL1 ->
  VmState ->
  Either ScriptError VmState
checkEcdsaMultiSig _txContext [] _pubKeys _code st =
  let res = boolToStackElement True
   in pure $ addBytesPushed res.byteSize $ st {s = st.s :|> res}
checkEcdsaMultiSig _txContext sigs pubKeys _code st
  | length sigs > length pubKeys =
      let res = boolToStackElement False
       in pure $ addBytesPushed res.byteSize $ st {s = st.s :|> res}
checkEcdsaMultiSig txContext (sig : sigs) (pubKey : pubKeys) code st = do
  verifyNotSchnorr
  checkRawEcdsaSignatureEncoding sig
  checkPubKeyEncoding pubKey
  let (res, st') = case checkSig txContext code sig pubKey of
        Just (res', imgSize) -> (res', addHashIterations imgSize True st)
        Nothing -> (False, st)
  if res
    then checkEcdsaMultiSig txContext sigs pubKeys code st'
    else checkEcdsaMultiSig txContext (sig : sigs) pubKeys code st'
  where
    verifyNotSchnorr :: Either ScriptError ()
    verifyNotSchnorr =
      when (B.length sig == schnorrSigSize) $ Left SeSigBadLength
checkEcdsaMultiSig _txContext _ [] _ _ = canNotHappen
