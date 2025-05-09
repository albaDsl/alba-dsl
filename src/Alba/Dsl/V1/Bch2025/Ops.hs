-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Ops where

import Alba.Dsl.V1.Bch2025.CompilerUtils (aop, pushIntegerCode)
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
opTrue (S c) = S (aop c OP_1)

opFalse :: FN s (s > TBool)
opFalse (S c) = S (aop c OP_0)

op0 :: FN s (s > TNat)
op0 (S c) = S (aop c OP_0)

op1 :: FN s (s > TNat)
op1 (S c) = S (aop c OP_1)

op2 :: FN s (s > TNat)
op2 (S c) = S (aop c OP_2)

op3 :: FN s (s > TNat)
op3 (S c) = S (aop c OP_3)

op4 :: FN s (s > TNat)
op4 (S c) = S (aop c OP_4)

op5 :: FN s (s > TNat)
op5 (S c) = S (aop c OP_5)

op6 :: FN s (s > TNat)
op6 (S c) = S (aop c OP_6)

op7 :: FN s (s > TNat)
op7 (S c) = S (aop c OP_7)

op8 :: FN s (s > TNat)
op8 (S c) = S (aop c OP_8)

op9 :: FN s (s > TNat)
op9 (S c) = S (aop c OP_9)

op10 :: FN s (s > TNat)
op10 (S c) = S (aop c OP_10)

op11 :: FN s (s > TNat)
op11 (S c) = S (aop c OP_11)

op12 :: FN s (s > TNat)
op12 (S c) = S (aop c OP_12)

op13 :: FN s (s > TNat)
op13 (S c) = S (aop c OP_13)

op14 :: FN s (s > TNat)
op14 (S c) = S (aop c OP_14)

op15 :: FN s (s > TNat)
op15 (S c) = S (aop c OP_15)

op16 :: FN s (s > TNat)
op16 (S c) = S (aop c OP_16)

op1Negate :: (StackInt x1) => FN s (s > x1)
op1Negate (S c) = S (aop c OP_1NEGATE)

opNop :: FN s s
opNop (S c) = S (aop c OP_NOP)

opIf ::
  FNA s alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > TBool) alt s' alt'
opIf ifOps elseOps (S c) =
  let (S c') = ifOps (S (aop c OP_IF))
      (S c'') = elseOps (S (aop c' OP_ELSE))
   in S (aop c'' OP_ENDIF)

opNotIf ::
  FNA s alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > TBool) alt s' alt'
opNotIf ifOps elseOps (S c) =
  let (S c') = ifOps (S (aop c OP_NOTIF))
      (S c'') = elseOps (S (aop c' OP_ELSE))
   in S (aop c'' OP_ENDIF)

opVerify :: FN (s > TBool) s
opVerify (S c) = S (aop c OP_VERIFY)

opReturn :: FN s s
opReturn (S c) = S (aop c OP_RETURN)

opToAltStack :: (StackEntry x1) => FNA (s > x1) alt s (alt > x1)
opToAltStack (S c) = S (aop c OP_TOALTSTACK)

opFromAltStack :: (StackEntry x1) => FNA s (alt > x1) (s > x1) alt
opFromAltStack (S c) = S (aop c OP_FROMALTSTACK)

opDepth :: FN s (s > TNat)
opDepth (S c) = S (aop c OP_DEPTH)

opDrop :: (StackEntry x1) => FN (s > x1) s
opDrop (S c) = S (aop c OP_DROP)

opDup :: (StackEntry x1) => FN (s > x1) (s > x1 > x1)
opDup (S c) = S (aop c OP_DUP)

opNip :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x2)
opNip (S c) = S (aop c OP_NIP)

opOver :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x1 > x2 > x1)
opOver (S c) = S (aop c OP_OVER)

opPick ::
  forall idx arg s.
  (KnownNat idx, StackEntry arg, Ref s idx ~ 'Just arg) =>
  FN s (s > arg)
opPick (S c) =
  let idx = natVal (Proxy :: Proxy idx) :: Integer
   in S (aop (c <> pushIntegerCode idx) OP_PICK)

opRoll ::
  forall idx arg s s'.
  (KnownNat idx, StackEntry arg, Ref s idx ~ 'Just arg, s' ~ Remove s idx) =>
  FN s (s' > arg)
opRoll (S c) =
  let idx = natVal (Proxy :: Proxy idx) :: Integer
   in S (aop (c <> pushIntegerCode idx) OP_ROLL)

opRot ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  FN (s > x1 > x2 > x3) (s > x2 > x3 > x1)
opRot (S c) = S (aop c OP_ROT)

opSwap :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x2 > x1)
opSwap (S c) = S (aop c OP_SWAP)

opTuck :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) (s > x2 > x1 > x2)
opTuck (S c) = S (aop c OP_TUCK)

op2Drop :: (StackEntry x1, StackEntry x2) => FN (s > x1 > x2) s
op2Drop (S c) = S (aop c OP_2DROP)

op2Dup ::
  (StackEntry x1, StackEntry x2) =>
  FN (s > x1 > x2) (s > x1 > x2 > x1 > x2)
op2Dup (S c) = S (aop c OP_2DUP)

op3Dup ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  FN (s > x1 > x2 > x3) (s > x1 > x2 > x3 > x1 > x2 > x3)
op3Dup (S c) = S (aop c OP_3DUP)

op2Over ::
  (StackEntry x1, StackEntry x2, StackEntry x3, StackEntry x4) =>
  FN (s > x1 > x2 > x3 > x4) (s > x1 > x2 > x3 > x4 > x1 > x2)
op2Over (S c) = S (aop c OP_2OVER)

op2Rot ::
  ( StackEntry x1,
    StackEntry x2,
    StackEntry x3,
    StackEntry x4,
    StackEntry x5,
    StackEntry x6
  ) =>
  FN (s > x1 > x2 > x3 > x4 > x5 > x6) (s > x3 > x4 > x5 > x6 > x1 > x2)
op2Rot (S c) = S (aop c OP_2ROT)

op2Swap ::
  (StackEntry x1, StackEntry x2, StackEntry x3, StackEntry x4) =>
  FN (s > x1 > x2 > x3 > x4) (s > x3 > x4 > x1 > x2)
op2Swap (S c) = S (aop c OP_2SWAP)

opCat :: (StackBytes x1, StackBytes x2) => FN (s > x1 > x2) (s > TBytes)
opCat (S c) = S (aop c OP_CAT)

opSplit ::
  (StackBytes x1, StackNat x2) =>
  FN
    (s > x1 > x2)
    (s > TBytes > TBytes)
opSplit (S c) = S (aop c OP_SPLIT)

opNum2Bin :: FN (s > TInt > TInt) (s > TBytes)
opNum2Bin (S c) = S (aop c OP_NUM2BIN)

opBin2Num :: FN (s > TBytes) (s > TInt)
opBin2Num (S c) = S (aop c OP_BIN2NUM)

opSize :: (StackBytes x1) => FN (s > x1) (s > x1 > TNat)
opSize (S c) = S (aop c OP_SIZE)

opAnd :: FN (s > TBytes > TBytes) (s > TBytes)
opAnd (S c) = S (aop c OP_AND)

opOr :: FN (s > TBytes > TBytes) (s > TBytes)
opOr (S c) = S (aop c OP_OR)

opXor :: FN (s > TBytes > TBytes) (s > TBytes)
opXor (S c) = S (aop c OP_XOR)

opReverseBytes :: (StackBytes x1) => FN (s > x1) (s > TBytes)
opReverseBytes (S c) = S (aop c OP_REVERSEBYTES)

opEqual :: (StackEntry x1) => FN (s > x1 > x1) (s > TBool)
opEqual (S c) = S (aop c OP_EQUAL)

opEqualVerify :: (StackEntry x1) => FN (s > x1 > x1) s
opEqualVerify (S c) = S (aop c OP_EQUALVERIFY)

op1Add :: (StackNum x1) => FN (s > x1) (s > x1)
op1Add (S c) = S (aop c OP_1ADD)

op1Sub :: (StackInt x1) => FN (s > x1) (s > x1)
op1Sub (S c) = S (aop c OP_1SUB)

opNegate :: (StackInt x1) => FN (s > x1) (s > x1)
opNegate (S c) = S (aop c OP_NEGATE)

opAbs :: (StackInt x1) => FN (s > x1) (s > x1)
opAbs (S c) = S (aop c OP_ABS)

opNot :: (StackBool x1) => FN (s > x1) (s > x1)
opNot (S c) = S (aop c OP_NOT)

op0NotEqual :: (StackNum x1) => FN (s > x1) (s > TBool)
op0NotEqual (S c) = S (aop c OP_0NOTEQUAL)

opAdd :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opAdd (S c) = S (aop c OP_ADD)

opSub :: (StackInt x1) => FN (s > x1 > x1) (s > x1)
opSub (S c) = S (aop c OP_SUB)

opSubUnsafe :: (StackNat x1) => FN (s > x1 > x1) (s > x1)
opSubUnsafe (S c) = S (aop c OP_SUB)

opMul :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMul (S c) = S (aop c OP_MUL)

opDiv :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opDiv (S c) = S (aop c OP_DIV)

opMod :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMod (S c) = S (aop c OP_MOD)

opBoolAnd :: (StackBool x1) => FN (s > x1 > x1) (s > x1)
opBoolAnd (S c) = S (aop c OP_BOOLAND)

opBoolOr :: (StackBool x1) => FN (s > x1 > x1) (s > x1)
opBoolOr (S c) = S (aop c OP_BOOLOR)

opNumEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opNumEqual (S c) = S (aop c OP_NUMEQUAL)

opNumEqualVerify :: (StackNum x1) => FN (s > x1 > x1) s
opNumEqualVerify (S c) = S (aop c OP_NUMEQUALVERIFY)

opNumNotEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opNumNotEqual (S c) = S (aop c OP_NUMNOTEQUAL)

opLessThan :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opLessThan (S c) = S (aop c OP_LESSTHAN)

opGreaterThan :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opGreaterThan (S c) = S (aop c OP_GREATERTHAN)

opLessThanOrEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opLessThanOrEqual (S c) = S (aop c OP_LESSTHANOREQUAL)

opGreaterThanOrEqual :: (StackNum x1) => FN (s > x1 > x1) (s > TBool)
opGreaterThanOrEqual (S c) = S (aop c OP_GREATERTHANOREQUAL)

opMin :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMin (S c) = S (aop c OP_MIN)

opMax :: (StackNum x1) => FN (s > x1 > x1) (s > x1)
opMax (S c) = S (aop c OP_MAX)

opWithin :: (StackNum x1) => FN (s > x1 > x1 > x1) (s > TBool)
opWithin (S c) = S (aop c OP_WITHIN)

opRipemd160 :: (StackBytes x1) => FN (s > x1) (s > TRipemd160)
opRipemd160 (S c) = S (aop c OP_RIPEMD160)

opSha1 :: (StackBytes x1) => FN (s > x1) (s > TSha1)
opSha1 (S c) = S (aop c OP_SHA1)

opSha256 :: (StackBytes x1) => FN (s > x1) (s > TSha256)
opSha256 (S c) = S (aop c OP_SHA256)

opHash160 :: (StackBytes x1) => FN (s > x1) (s > THash160)
opHash160 (S c) = S (aop c OP_HASH160)

opHash256 :: (StackBytes x1) => FN (s > x1) (s > THash256)
opHash256 (S c) = S (aop c OP_HASH256)

opCodeSeparator :: FNC
opCodeSeparator (S c) = S (aop c OP_CODESEPARATOR)

opCheckSig :: FN (s > TSig > TPubKey) (s > TBool)
opCheckSig (S c) = S (aop c OP_CHECKSIG)

opCheckSigVerify :: FN (s > TSig > TPubKey) s
opCheckSigVerify (S c) = S (aop c OP_CHECKSIGVERIFY)

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
opCheckMultiSig (S c) = S (aop c OP_CHECKMULTISIG)

opCheckMultiSigVerify ::
  forall numSigs numKeys s.
  (KnownNat numSigs, KnownNat numKeys) =>
  FN (MultiSigInputStack s numSigs numKeys) s
opCheckMultiSigVerify (S c) = S (aop c OP_CHECKMULTISIGVERIFY)

opCheckDataSig :: FN (s > TSig > TBytes > TPubKey) (s > TBool)
opCheckDataSig (S c) = S (aop c OP_CHECKDATASIG)

opCheckDataSigVerify :: FN (s > TSig > TBytes > TPubKey) s
opCheckDataSigVerify (S c) = S (aop c OP_CHECKDATASIGVERIFY)

opCheckLockTimeVerify :: FN (s > TNat) (s > TNat)
opCheckLockTimeVerify (S c) = S (aop c OP_CHECKLOCKTIMEVERIFY)

opCheckSequenceVerify :: FN (s > TNat) (s > TNat)
opCheckSequenceVerify (S c) = S (aop c OP_CHECKSEQUENCEVERIFY)

opInputIndex :: FN s (s > TNat)
opInputIndex (S c) = S (aop c OP_INPUTINDEX)

opActiveBytecode :: FN s (s > TBytes)
opActiveBytecode (S c) = S (aop c OP_ACTIVEBYTECODE)

opTxVersion :: FN s (s > TNat)
opTxVersion (S c) = S (aop c OP_TXVERSION)

opTxInputCount :: FN s (s > TNat)
opTxInputCount (S c) = S (aop c OP_TXINPUTCOUNT)

opTxOutputCount :: FN s (s > TNat)
opTxOutputCount (S c) = S (aop c OP_TXOUTPUTCOUNT)

opTxLockTime :: FN s (s > TNat)
opTxLockTime (S c) = S (aop c OP_TXLOCKTIME)

opUtxoValue :: FN (s > TNat) (s > TNat)
opUtxoValue (S c) = S (aop c OP_UTXOVALUE)

opUtxoBytecode :: FN (s > TNat) (s > TBytes)
opUtxoBytecode (S c) = S (aop c OP_UTXOBYTECODE)

opUtxoTokenCategory :: FN (s > TNat) (s > TBytes)
opUtxoTokenCategory (S c) = S (aop c OP_UTXOTOKENCATEGORY)

opUtxoTokenCommitment :: FN (s > TNat) (s > TBytes)
opUtxoTokenCommitment (S c) = S (aop c OP_UTXOTOKENCOMMITMENT)

opUtxoTokenAmount :: FN (s > TNat) (s > TNat)
opUtxoTokenAmount (S c) = S (aop c OP_UTXOTOKENAMOUNT)

opOutPointTxHash :: FN (s > TNat) (s > THash256)
opOutPointTxHash (S c) = S (aop c OP_OUTPOINTTXHASH)

opOutPointIndex :: FN (s > TNat) (s > TNat)
opOutPointIndex (S c) = S (aop c OP_OUTPOINTINDEX)

opInputBytecode :: FN (s > TNat) (s > TBytes)
opInputBytecode (S c) = S (aop c OP_INPUTBYTECODE)

opInputSequenceNumber :: FN (s > TNat) (s > TNat)
opInputSequenceNumber (S c) = S (aop c OP_INPUTSEQUENCENUMBER)

opOutputValue :: FN (s > TNat) (s > TNat)
opOutputValue (S c) = S (aop c OP_OUTPUTVALUE)

opOutputBytecode :: FN (s > TNat) (s > TBytes)
opOutputBytecode (S c) = S (aop c OP_OUTPUTBYTECODE)

opOutputTokenCategory :: FN (s > TNat) (s > TBytes)
opOutputTokenCategory (S c) = S (aop c OP_OUTPUTTOKENCATEGORY)

opOutputTokenCommitment :: FN (s > TNat) (s > TBytes)
opOutputTokenCommitment (S c) = S (aop c OP_OUTPUTTOKENCOMMITMENT)

opOutputTokenAmount :: FN (s > TNat) (s > TNat)
opOutputTokenAmount (S c) = S (aop c OP_OUTPUTTOKENAMOUNT)
