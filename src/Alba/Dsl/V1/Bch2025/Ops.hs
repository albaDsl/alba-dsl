-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Ops where

import Alba.Dsl.V1.Bch2025.Stack
  ( StackBool,
    StackBytes,
    StackEntry,
    StackInt,
    StackNat,
    StackNum,
    THash160,
    THash256,
    TRipemd160,
    TSha1,
    TSha256,
  )
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops, pushIntegerOp)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack
  ( FN,
    FNA,
    FNC,
    Ref,
    Remove,
    S (S),
    TBool,
    TBytes,
    TInt,
    TNat,
    TPubKey,
    TSig,
  )
import Alba.Dsl.V1.Common.TypeFamilies (Append, Replicate)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

opTrue :: FN s (s > TBool)
opTrue (S c fs) = S (aop c OP_1) fs

opFalse :: FN s (s > TBool)
opFalse (S c fs) = S (aop c OP_0) fs

op0 :: (StackNum x1) => FN s (s > x1)
op0 (S c fs) = S (aop c OP_0) fs

op1 :: (StackNum x1) => FN s (s > x1)
op1 (S c fs) = S (aop c OP_1) fs

op2 :: (StackNum x1) => FN s (s > x1)
op2 (S c fs) = S (aop c OP_2) fs

op3 :: (StackNum x1) => FN s (s > x1)
op3 (S c fs) = S (aop c OP_3) fs

op4 :: (StackNum x1) => FN s (s > x1)
op4 (S c fs) = S (aop c OP_4) fs

op5 :: (StackNum x1) => FN s (s > x1)
op5 (S c fs) = S (aop c OP_5) fs

op6 :: (StackNum x1) => FN s (s > x1)
op6 (S c fs) = S (aop c OP_6) fs

op7 :: (StackNum x1) => FN s (s > x1)
op7 (S c fs) = S (aop c OP_7) fs

op8 :: (StackNum x1) => FN s (s > x1)
op8 (S c fs) = S (aop c OP_8) fs

op9 :: (StackNum x1) => FN s (s > x1)
op9 (S c fs) = S (aop c OP_9) fs

op10 :: (StackNum x1) => FN s (s > x1)
op10 (S c fs) = S (aop c OP_10) fs

op11 :: (StackNum x1) => FN s (s > x1)
op11 (S c fs) = S (aop c OP_11) fs

op12 :: (StackNum x1) => FN s (s > x1)
op12 (S c fs) = S (aop c OP_12) fs

op13 :: (StackNum x1) => FN s (s > x1)
op13 (S c fs) = S (aop c OP_13) fs

op14 :: (StackNum x1) => FN s (s > x1)
op14 (S c fs) = S (aop c OP_14) fs

op15 :: (StackNum x1) => FN s (s > x1)
op15 (S c fs) = S (aop c OP_15) fs

op16 :: (StackNum x1) => FN s (s > x1)
op16 (S c fs) = S (aop c OP_16) fs

op1Negate :: (StackInt x1) => FN s (s > x1)
op1Negate (S c fs) = S (aop c OP_1NEGATE) fs

opNop :: FN s s
opNop (S c fs) = S (aop c OP_NOP) fs

opIf ::
  FNA s alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > TBool) alt s' alt'
opIf ifOps elseOps (S c fs) =
  let (S c' fs') = ifOps (S (aop c OP_IF) fs)
      (S c'' fs'') = elseOps (S (aop c' OP_ELSE) fs')
   in S (aop c'' OP_ENDIF) fs''

-- Version of opIf without the else clause.
opWhen :: FNA s alt s alt -> FNA (s > TBool) alt s alt
opWhen body (S c fs) =
  let (S c' fs') = body (S (aop c OP_IF) fs)
   in S (aop c' OP_ENDIF) fs'

opNotIf ::
  FNA s alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > TBool) alt s' alt'
opNotIf ifOps elseOps (S c fs) =
  let (S c' fs') = ifOps (S (aop c OP_NOTIF) fs)
      (S c'' fs'') = elseOps (S (aop c' OP_ELSE) fs')
   in S (aop c'' OP_ENDIF) fs''

-- Version of opNotIf without the else clause.
opUnless :: FNA s alt s alt -> FNA (s > TBool) alt s alt
opUnless body (S c fs) =
  let (S c' fs') = body (S (aop c OP_NOTIF) fs)
   in S (aop c' OP_ENDIF) fs'

opVerify :: FN (s > TBool) s
opVerify (S c fs) = S (aop c OP_VERIFY) fs

opReturn :: FN s s
opReturn (S c fs) = S (aop c OP_RETURN) fs

opToAltStack :: (StackEntry x1) => FNA (s > x1) alt s (alt > x1)
opToAltStack (S c fs) = S (aop c OP_TOALTSTACK) fs

opFromAltStack :: (StackEntry x1) => FNA s (alt > x1) (s > x1) alt
opFromAltStack (S c fs) = S (aop c OP_FROMALTSTACK) fs

opDepth :: FN s (s > TNat)
opDepth (S c fs) = S (aop c OP_DEPTH) fs

opDrop :: (StackEntry x1) => FN (s > x1) s
opDrop (S c fs) = S (aop c OP_DROP) fs

opDup :: (StackEntry x1) => FN (s > x1) (s > x1 > x1)
opDup (S c fs) = S (aop c OP_DUP) fs

opNip :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x2)
opNip (S c fs) = S (aop c OP_NIP) fs

opOver :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x1 > x2 > x1)
opOver (S c fs) = S (aop c OP_OVER) fs

opPick ::
  forall idx arg s.
  (KnownNat idx, StackEntry arg, Ref s idx ~ 'Just arg) =>
  FN s (s > arg)
opPick (S c fs) =
  let idx = natVal (Proxy :: Proxy idx) :: Integer
   in S (aops c [pushIntegerOp idx, OP_PICK]) fs

opRoll ::
  forall idx arg s s'.
  (KnownNat idx, StackEntry arg, Ref s idx ~ 'Just arg, s' ~ Remove s idx) =>
  FN s (s' > arg)
opRoll (S c fs) =
  let idx = natVal (Proxy :: Proxy idx) :: Integer
   in S (aops c [pushIntegerOp idx, OP_ROLL]) fs

opRot ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  FN (s > x1 > x2 > x3) (s > x2 > x3 > x1)
opRot (S c fs) = S (aop c OP_ROT) fs

opSwap :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x2 > x1)
opSwap (S c fs) = S (aop c OP_SWAP) fs

opTuck :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x2 > x1 > x2)
opTuck (S c fs) = S (aop c OP_TUCK) fs

op2Drop :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) s
op2Drop (S c fs) = S (aop c OP_2DROP) fs

op2Dup ::
  (StackEntry x1, StackEntry x2) =>
  FN (s > x1 > x2) (s > x1 > x2 > x1 > x2)
op2Dup (S c fs) = S (aop c OP_2DUP) fs

op3Dup ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  FN (s > x1 > x2 > x3) (s > x1 > x2 > x3 > x1 > x2 > x3)
op3Dup (S c fs) = S (aop c OP_3DUP) fs

op2Over ::
  (StackEntry x1, StackEntry x2, StackEntry x3, StackEntry x4) =>
  FN (s > x1 > x2 > x3 > x4) (s > x1 > x2 > x3 > x4 > x1 > x2)
op2Over (S c fs) = S (aop c OP_2OVER) fs

op2Rot ::
  ( StackEntry x1,
    StackEntry x2,
    StackEntry x3,
    StackEntry x4,
    StackEntry x5,
    StackEntry x6
  ) =>
  FN (s > x1 > x2 > x3 > x4 > x5 > x6) (s > x3 > x4 > x5 > x6 > x1 > x2)
op2Rot (S c fs) = S (aop c OP_2ROT) fs

op2Swap ::
  (StackEntry x1, StackEntry x2, StackEntry x3, StackEntry x4) =>
  FN (s > x1 > x2 > x3 > x4) (s > x3 > x4 > x1 > x2)
op2Swap (S c fs) = S (aop c OP_2SWAP) fs

opCat :: (StackBytes x1, StackBytes x2) => FN (s > x1 > x2) (s > TBytes)
opCat (S c fs) = S (aop c OP_CAT) fs

opSplit ::
  (StackBytes x1, StackNat x2) =>
  FN
    (s > x1 > x2)
    (s > TBytes > TBytes)
opSplit (S c fs) = S (aop c OP_SPLIT) fs

opNum2Bin :: FN (s > TInt > TNat) (s > TBytes)
opNum2Bin (S c fs) = S (aop c OP_NUM2BIN) fs

opBin2Num :: FN (s > TBytes) (s > TInt)
opBin2Num (S c fs) = S (aop c OP_BIN2NUM) fs

opSize :: (StackBytes x1) => FN (s > x1) (s > x1 > TNat)
opSize (S c fs) = S (aop c OP_SIZE) fs

opAnd :: FN (s > TBytes > TBytes) (s > TBytes)
opAnd (S c fs) = S (aop c OP_AND) fs

opOr :: FN (s > TBytes > TBytes) (s > TBytes)
opOr (S c fs) = S (aop c OP_OR) fs

opXor :: FN (s > TBytes > TBytes) (s > TBytes)
opXor (S c fs) = S (aop c OP_XOR) fs

opReverseBytes :: (StackBytes x1) => FN (s > x1) (s > TBytes)
opReverseBytes (S c fs) = S (aop c OP_REVERSEBYTES) fs

opEqual :: (StackEntry x1) => FN (s > x1 > x1) (s > TBool)
opEqual (S c fs) = S (aop c OP_EQUAL) fs

opEqualVerify :: (StackEntry x1) => FN (s > x1 > x1) s
opEqualVerify (S c fs) = S (aop c OP_EQUALVERIFY) fs

op1Add :: (StackNum x1) => FN (s > x1) (s > x1)
op1Add (S c fs) = S (aop c OP_1ADD) fs

op1Sub :: (StackInt x1) => FN (s > x1) (s > x1)
op1Sub (S c fs) = S (aop c OP_1SUB) fs

op1SubUnsafe :: (StackNat x1) => FN (s > x1) (s > x1)
op1SubUnsafe (S c fs) = S (aop c OP_1SUB) fs

opNegate :: (StackInt x1) => FN (s > x1) (s > x1)
opNegate (S c fs) = S (aop c OP_NEGATE) fs

opAbs :: (StackInt x1) => FN (s > x1) (s > x1)
opAbs (S c fs) = S (aop c OP_ABS) fs

opNot :: (StackBool x1) => FN (s > x1) (s > x1)
opNot (S c fs) = S (aop c OP_NOT) fs

op0NotEqual :: (StackNum x1) => FN (s > x1) (s > TBool)
op0NotEqual (S c fs) = S (aop c OP_0NOTEQUAL) fs

opAdd :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opAdd (S c fs) = S (aop c OP_ADD) fs

opSub :: (StackInt x1) => FN (s > x1 > x1) (s > x1)
opSub (S c fs) = S (aop c OP_SUB) fs

opSubUnsafe :: (StackNat x1) => FN (s > x1 > x1) (s > x1)
opSubUnsafe (S c fs) = S (aop c OP_SUB) fs

opMul :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMul (S c fs) = S (aop c OP_MUL) fs

opDiv :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opDiv (S c fs) = S (aop c OP_DIV) fs

opMod :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMod (S c fs) = S (aop c OP_MOD) fs

opBoolAnd :: (StackBool x1) => FN (s > x1 > x1) (s > x1)
opBoolAnd (S c fs) = S (aop c OP_BOOLAND) fs

opBoolOr :: (StackBool x1) => FN (s > x1 > x1) (s > x1)
opBoolOr (S c fs) = S (aop c OP_BOOLOR) fs

opNumEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opNumEqual (S c fs) = S (aop c OP_NUMEQUAL) fs

opNumEqualVerify :: (StackNum x1) => FN (s > x1 > x1) s
opNumEqualVerify (S c fs) = S (aop c OP_NUMEQUALVERIFY) fs

opNumNotEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opNumNotEqual (S c fs) = S (aop c OP_NUMNOTEQUAL) fs

opLessThan :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opLessThan (S c fs) = S (aop c OP_LESSTHAN) fs

opGreaterThan :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opGreaterThan (S c fs) = S (aop c OP_GREATERTHAN) fs

opLessThanOrEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opLessThanOrEqual (S c fs) = S (aop c OP_LESSTHANOREQUAL) fs

opGreaterThanOrEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opGreaterThanOrEqual (S c fs) = S (aop c OP_GREATERTHANOREQUAL) fs

opMin :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMin (S c fs) = S (aop c OP_MIN) fs

opMax :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMax (S c fs) = S (aop c OP_MAX) fs

opWithin :: (StackNum x1) => FN (s > x1 > x1 > x1) (s > TBool)
opWithin (S c fs) = S (aop c OP_WITHIN) fs

opRipemd160 :: (StackBytes x1) => FN (s > x1) (s > TRipemd160)
opRipemd160 (S c fs) = S (aop c OP_RIPEMD160) fs

opSha1 :: (StackBytes x1) => FN (s > x1) (s > TSha1)
opSha1 (S c fs) = S (aop c OP_SHA1) fs

opSha256 :: (StackBytes x1) => FN (s > x1) (s > TSha256)
opSha256 (S c fs) = S (aop c OP_SHA256) fs

opHash160 :: (StackBytes x1) => FN (s > x1) (s > THash160)
opHash160 (S c fs) = S (aop c OP_HASH160) fs

opHash256 :: (StackBytes x1) => FN (s > x1) (s > THash256)
opHash256 (S c fs) = S (aop c OP_HASH256) fs

opCodeSeparator :: FNC
opCodeSeparator (S c fs) = S (aop c OP_CODESEPARATOR) fs

opCheckSig :: FN (s > TSig > TPubKey) (s > TBool)
opCheckSig (S c fs) = S (aop c OP_CHECKSIG) fs

opCheckSigVerify :: FN (s > TSig > TPubKey) s
opCheckSigVerify (S c fs) = S (aop c OP_CHECKSIGVERIFY) fs

type MultiSigInputStack s numSigs numKeys =
  ( Append
      (s > TNat)
      ( Append
          (Append (Replicate numSigs TSig) '[TNat])
          (Append (Replicate numKeys TPubKey) '[TNat])
      )
  )

opCheckMultiSig ::
  forall numSigs numKeys s.
  (KnownNat numSigs, KnownNat numKeys) =>
  FN (MultiSigInputStack s numSigs numKeys) (s > TBool)
opCheckMultiSig (S c fs) = S (aop c OP_CHECKMULTISIG) fs

opCheckMultiSigVerify ::
  forall numSigs numKeys s.
  (KnownNat numSigs, KnownNat numKeys) =>
  FN (MultiSigInputStack s numSigs numKeys) s
opCheckMultiSigVerify (S c fs) = S (aop c OP_CHECKMULTISIGVERIFY) fs

opCheckDataSig :: FN (s > TSig > TBytes > TPubKey) (s > TBool)
opCheckDataSig (S c fs) = S (aop c OP_CHECKDATASIG) fs

opCheckDataSigVerify :: FN (s > TSig > TBytes > TPubKey) s
opCheckDataSigVerify (S c fs) = S (aop c OP_CHECKDATASIGVERIFY) fs

opCheckLockTimeVerify :: FN (s > TNat) (s > TNat)
opCheckLockTimeVerify (S c fs) = S (aop c OP_CHECKLOCKTIMEVERIFY) fs

opCheckSequenceVerify :: FN (s > TNat) (s > TNat)
opCheckSequenceVerify (S c fs) = S (aop c OP_CHECKSEQUENCEVERIFY) fs

opInputIndex :: FN s (s > TNat)
opInputIndex (S c fs) = S (aop c OP_INPUTINDEX) fs

opActiveBytecode :: FN s (s > TBytes)
opActiveBytecode (S c fs) = S (aop c OP_ACTIVEBYTECODE) fs

opTxVersion :: FN s (s > TNat)
opTxVersion (S c fs) = S (aop c OP_TXVERSION) fs

opTxInputCount :: FN s (s > TNat)
opTxInputCount (S c fs) = S (aop c OP_TXINPUTCOUNT) fs

opTxOutputCount :: FN s (s > TNat)
opTxOutputCount (S c fs) = S (aop c OP_TXOUTPUTCOUNT) fs

opTxLockTime :: FN s (s > TNat)
opTxLockTime (S c fs) = S (aop c OP_TXLOCKTIME) fs

opUtxoValue :: FN (s > TNat) (s > TNat)
opUtxoValue (S c fs) = S (aop c OP_UTXOVALUE) fs

opUtxoBytecode :: FN (s > TNat) (s > TBytes)
opUtxoBytecode (S c fs) = S (aop c OP_UTXOBYTECODE) fs

opUtxoTokenCategory :: FN (s > TNat) (s > TBytes)
opUtxoTokenCategory (S c fs) = S (aop c OP_UTXOTOKENCATEGORY) fs

opUtxoTokenCommitment :: FN (s > TNat) (s > TBytes)
opUtxoTokenCommitment (S c fs) = S (aop c OP_UTXOTOKENCOMMITMENT) fs

opUtxoTokenAmount :: FN (s > TNat) (s > TNat)
opUtxoTokenAmount (S c fs) = S (aop c OP_UTXOTOKENAMOUNT) fs

opOutPointTxHash :: FN (s > TNat) (s > THash256)
opOutPointTxHash (S c fs) = S (aop c OP_OUTPOINTTXHASH) fs

opOutPointIndex :: FN (s > TNat) (s > TNat)
opOutPointIndex (S c fs) = S (aop c OP_OUTPOINTINDEX) fs

opInputBytecode :: FN (s > TNat) (s > TBytes)
opInputBytecode (S c fs) = S (aop c OP_INPUTBYTECODE) fs

opInputSequenceNumber :: FN (s > TNat) (s > TNat)
opInputSequenceNumber (S c fs) = S (aop c OP_INPUTSEQUENCENUMBER) fs

opOutputValue :: FN (s > TNat) (s > TNat)
opOutputValue (S c fs) = S (aop c OP_OUTPUTVALUE) fs

opOutputBytecode :: FN (s > TNat) (s > TBytes)
opOutputBytecode (S c fs) = S (aop c OP_OUTPUTBYTECODE) fs

opOutputTokenCategory :: FN (s > TNat) (s > TBytes)
opOutputTokenCategory (S c fs) = S (aop c OP_OUTPUTTOKENCATEGORY) fs

opOutputTokenCommitment :: FN (s > TNat) (s > TBytes)
opOutputTokenCommitment (S c fs) = S (aop c OP_OUTPUTTOKENCOMMITMENT) fs

opOutputTokenAmount :: FN (s > TNat) (s > TNat)
opOutputTokenAmount (S c fs) = S (aop c OP_OUTPUTTOKENAMOUNT) fs
