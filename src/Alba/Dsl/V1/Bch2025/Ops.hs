-- Copyright (c) 2025 albaDsl
{-# LANGUAGE RequiredTypeArguments #-}

module Alba.Dsl.V1.Bch2025.Ops where

import Alba.Dsl.V1.Bch2025.Stack
  ( StackBool,
    StackBytes,
    StackEntry,
    StackEquatable,
    StackInt,
    StackNat,
    StackNum,
    THash160,
    THash256,
    TRipemd160,
    TSha1,
    TSha256,
  )
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops, integerToDataOp)
import Alba.Dsl.V1.Common.Stack
  ( Append,
    Fn,
    FnA,
    FnC,
    Ref,
    Remove,
    Replicate,
    Stack (..),
    TBool,
    TBytes,
    TInt,
    TNat,
    TPubKey,
    TSig,
  )
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Control.Arrow ((>>>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

opTrue :: Fn s (s :> TBool)
opTrue = aop OP_1

opFalse :: Fn s (s :> TBool)
opFalse = aop OP_0

op0 :: (StackNum x1) => Fn s (s :> x1)
op0 = aop OP_0

op1 :: (StackNum x1) => Fn s (s :> x1)
op1 = aop OP_1

op2 :: (StackNum x1) => Fn s (s :> x1)
op2 = aop OP_2

op3 :: (StackNum x1) => Fn s (s :> x1)
op3 = aop OP_3

op4 :: (StackNum x1) => Fn s (s :> x1)
op4 = aop OP_4

op5 :: (StackNum x1) => Fn s (s :> x1)
op5 = aop OP_5

op6 :: (StackNum x1) => Fn s (s :> x1)
op6 = aop OP_6

op7 :: (StackNum x1) => Fn s (s :> x1)
op7 = aop OP_7

op8 :: (StackNum x1) => Fn s (s :> x1)
op8 = aop OP_8

op9 :: (StackNum x1) => Fn s (s :> x1)
op9 = aop OP_9

op10 :: (StackNum x1) => Fn s (s :> x1)
op10 = aop OP_10

op11 :: (StackNum x1) => Fn s (s :> x1)
op11 = aop OP_11

op12 :: (StackNum x1) => Fn s (s :> x1)
op12 = aop OP_12

op13 :: (StackNum x1) => Fn s (s :> x1)
op13 = aop OP_13

op14 :: (StackNum x1) => Fn s (s :> x1)
op14 = aop OP_14

op15 :: (StackNum x1) => Fn s (s :> x1)
op15 = aop OP_15

op16 :: (StackNum x1) => Fn s (s :> x1)
op16 = aop OP_16

op1Negate :: (StackInt x1) => Fn s (s :> x1)
op1Negate = aop OP_1NEGATE

opNop :: Fn s s
opNop = aop OP_NOP

opIf ::
  FnA s alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s :> TBool) alt s' alt'
opIf ifOps elseOps =
  aop OP_IF >>> ifOps >>> aop OP_ELSE >>> elseOps >>> aop OP_ENDIF

-- Version of opIf without the else clause.
opWhen :: FnA s alt s alt -> FnA (s :> TBool) alt s alt
opWhen body = aop OP_IF >>> body >>> aop OP_ENDIF

opNotIf ::
  FnA s alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s :> TBool) alt s' alt'
opNotIf ifOps elseOps =
  aop OP_NOTIF >>> ifOps >>> aop OP_ELSE >>> elseOps >>> aop OP_ENDIF

-- Version of opNotIf without the else clause.
opUnless :: FnA s alt s alt -> FnA (s :> TBool) alt s alt
opUnless body = aop OP_NOTIF >>> body >>> aop OP_ENDIF

opVerify :: Fn (s :> TBool) s
opVerify = aop OP_VERIFY

opReturn :: Fn s s
opReturn = aop OP_RETURN

opToAltStack :: (StackEntry x1) => FnA (s :> x1) alt s (alt :> x1)
opToAltStack = aop OP_TOALTSTACK

opFromAltStack :: (StackEntry x1) => FnA s (alt :> x1) (s :> x1) alt
opFromAltStack = aop OP_FROMALTSTACK

opDepth :: Fn s (s :> TNat)
opDepth = aop OP_DEPTH

opDrop :: (StackEntry x1) => Fn (s :> x1) s
opDrop = aop OP_DROP

opDup :: (StackEntry x1) => Fn (s :> x1) (s :> x1 :> x1)
opDup = aop OP_DUP

opNip :: (StackEntry x1, StackEntry x2) => Fn (s :> x1 :> x2) (s :> x2)
opNip = aop OP_NIP

opOver ::
  (StackEntry x1, StackEntry x2) =>
  Fn (s :> x1 :> x2) (s :> x1 :> x2 :> x1)
opOver = aop OP_OVER

opPick ::
  forall arg s.
  forall idx ->
  (KnownNat idx, StackEntry arg, Ref s idx ~ 'Just arg) =>
  Fn s (s :> arg)
opPick idx = aops [integerToDataOp (natVal (Proxy :: Proxy idx)), OP_PICK]

opRoll ::
  forall arg s s'.
  forall idx ->
  (KnownNat idx, StackEntry arg, Ref s idx ~ 'Just arg, s' ~ Remove s idx) =>
  Fn s (s' :> arg)
opRoll idx = aops [integerToDataOp (natVal (Proxy :: Proxy idx)), OP_ROLL]

opRot ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  Fn (s :> x1 :> x2 :> x3) (s :> x2 :> x3 :> x1)
opRot = aop OP_ROT

opSwap :: (StackEntry x1, StackEntry x2) => Fn (s :> x1 :> x2) (s :> x2 :> x1)
opSwap = aop OP_SWAP

opTuck ::
  (StackEntry x1, StackEntry x2) =>
  Fn (s :> x1 :> x2) (s :> x2 :> x1 :> x2)
opTuck = aop OP_TUCK

op2Drop :: (StackEntry x1, StackEntry x2) => Fn (s :> x1 :> x2) s
op2Drop = aop OP_2DROP

op2Dup ::
  (StackEntry x1, StackEntry x2) =>
  Fn (s :> x1 :> x2) (s :> x1 :> x2 :> x1 :> x2)
op2Dup = aop OP_2DUP

op3Dup ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  Fn (s :> x1 :> x2 :> x3) (s :> x1 :> x2 :> x3 :> x1 :> x2 :> x3)
op3Dup = aop OP_3DUP

op2Over ::
  (StackEntry x1, StackEntry x2, StackEntry x3, StackEntry x4) =>
  Fn (s :> x1 :> x2 :> x3 :> x4) (s :> x1 :> x2 :> x3 :> x4 :> x1 :> x2)
op2Over = aop OP_2OVER

op2Rot ::
  ( StackEntry x1,
    StackEntry x2,
    StackEntry x3,
    StackEntry x4,
    StackEntry x5,
    StackEntry x6
  ) =>
  Fn
    (s :> x1 :> x2 :> x3 :> x4 :> x5 :> x6)
    (s :> x3 :> x4 :> x5 :> x6 :> x1 :> x2)
op2Rot = aop OP_2ROT

op2Swap ::
  (StackEntry x1, StackEntry x2, StackEntry x3, StackEntry x4) =>
  Fn (s :> x1 :> x2 :> x3 :> x4) (s :> x3 :> x4 :> x1 :> x2)
op2Swap = aop OP_2SWAP

opCat :: (StackBytes x1, StackBytes x2) => Fn (s :> x1 :> x2) (s :> TBytes)
opCat = aop OP_CAT

opSplit ::
  (StackBytes x1, StackNat x2) =>
  Fn
    (s :> x1 :> x2)
    (s :> TBytes :> TBytes)
opSplit = aop OP_SPLIT

opNum2Bin :: Fn (s :> TInt :> TNat) (s :> TBytes)
opNum2Bin = aop OP_NUM2BIN

opBin2Num :: Fn (s :> TBytes) (s :> TInt)
opBin2Num = aop OP_BIN2NUM

opSize :: (StackBytes x1) => Fn (s :> x1) (s :> x1 :> TNat)
opSize = aop OP_SIZE

opAnd :: Fn (s :> TBytes :> TBytes) (s :> TBytes)
opAnd = aop OP_AND

opOr :: Fn (s :> TBytes :> TBytes) (s :> TBytes)
opOr = aop OP_OR

opXor :: Fn (s :> TBytes :> TBytes) (s :> TBytes)
opXor = aop OP_XOR

opReverseBytes :: (StackBytes x1) => Fn (s :> x1) (s :> TBytes)
opReverseBytes = aop OP_REVERSEBYTES

opEqual :: (StackEquatable x1) => Fn (s :> x1 :> x1) (s :> TBool)
opEqual = aop OP_EQUAL

opEqualVerify :: (StackEquatable x1) => Fn (s :> x1 :> x1) s
opEqualVerify = aop OP_EQUALVERIFY

op1Add :: (StackNum x1) => Fn (s :> x1) (s :> x1)
op1Add = aop OP_1ADD

op1Sub :: (StackInt x1) => Fn (s :> x1) (s :> x1)
op1Sub = aop OP_1SUB

opNegate :: (StackInt x1) => Fn (s :> x1) (s :> x1)
opNegate = aop OP_NEGATE

opAbs :: (StackInt x1) => Fn (s :> x1) (s :> x1)
opAbs = aop OP_ABS

opNot :: (StackBool x1) => Fn (s :> x1) (s :> x1)
opNot = aop OP_NOT

op0NotEqual :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
op0NotEqual = aop OP_0NOTEQUAL

opAdd :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> x1)
opAdd = aop OP_ADD

opSub :: (StackInt x1) => Fn (s :> x1 :> x1) (s :> x1)
opSub = aop OP_SUB

opMul :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> x1)
opMul = aop OP_MUL

opDiv :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> x1)
opDiv = aop OP_DIV

opMod :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> x1)
opMod = aop OP_MOD

opBoolAnd :: (StackBool x1) => Fn (s :> x1 :> x1) (s :> x1)
opBoolAnd = aop OP_BOOLAND

opBoolOr :: (StackBool x1) => Fn (s :> x1 :> x1) (s :> x1)
opBoolOr = aop OP_BOOLOR

opNumEqual :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> TBool)
opNumEqual = aop OP_NUMEQUAL

opNumEqualVerify :: (StackNum x1) => Fn (s :> x1 :> x1) s
opNumEqualVerify = aop OP_NUMEQUALVERIFY

opNumNotEqual :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> TBool)
opNumNotEqual = aop OP_NUMNOTEQUAL

opLessThan :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> TBool)
opLessThan = aop OP_LESSTHAN

opGreaterThan :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> TBool)
opGreaterThan = aop OP_GREATERTHAN

opLessThanOrEqual :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> TBool)
opLessThanOrEqual = aop OP_LESSTHANOREQUAL

opGreaterThanOrEqual :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> TBool)
opGreaterThanOrEqual = aop OP_GREATERTHANOREQUAL

opMin :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> x1)
opMin = aop OP_MIN

opMax :: (StackNum x1) => Fn (s :> x1 :> x1) (s :> x1)
opMax = aop OP_MAX

opWithin :: (StackNum x1) => Fn (s :> x1 :> x1 :> x1) (s :> TBool)
opWithin = aop OP_WITHIN

opRipemd160 :: (StackBytes x1) => Fn (s :> x1) (s :> TRipemd160)
opRipemd160 = aop OP_RIPEMD160

opSha1 :: (StackBytes x1) => Fn (s :> x1) (s :> TSha1)
opSha1 = aop OP_SHA1

opSha256 :: (StackBytes x1) => Fn (s :> x1) (s :> TSha256)
opSha256 = aop OP_SHA256

opHash160 :: (StackBytes x1) => Fn (s :> x1) (s :> THash160)
opHash160 = aop OP_HASH160

opHash256 :: (StackBytes x1) => Fn (s :> x1) (s :> THash256)
opHash256 = aop OP_HASH256

opCodeSeparator :: FnC
opCodeSeparator = aop OP_CODESEPARATOR

opCheckSig :: Fn (s :> TSig :> TPubKey) (s :> TBool)
opCheckSig = aop OP_CHECKSIG

opCheckSigVerify :: Fn (s :> TSig :> TPubKey) s
opCheckSigVerify = aop OP_CHECKSIGVERIFY

type MultiSigInputStack s numSigs numKeys =
  ( Append
      (s :> TNat)
      ( Append
          (Append (Replicate numSigs TSig) (Base :> TNat))
          (Append (Replicate numKeys TPubKey) (Base :> TNat))
      )
  )

opCheckMultiSig ::
  forall numSigs numKeys s.
  (KnownNat numSigs, KnownNat numKeys) =>
  Fn (MultiSigInputStack s numSigs numKeys) (s :> TBool)
opCheckMultiSig = aop OP_CHECKMULTISIG

opCheckMultiSigVerify ::
  forall numSigs numKeys s.
  (KnownNat numSigs, KnownNat numKeys) =>
  Fn (MultiSigInputStack s numSigs numKeys) s
opCheckMultiSigVerify = aop OP_CHECKMULTISIGVERIFY

opCheckDataSig :: Fn (s :> TSig :> TBytes :> TPubKey) (s :> TBool)
opCheckDataSig = aop OP_CHECKDATASIG

opCheckDataSigVerify :: Fn (s :> TSig :> TBytes :> TPubKey) s
opCheckDataSigVerify = aop OP_CHECKDATASIGVERIFY

opCheckLockTimeVerify :: Fn (s :> TNat) (s :> TNat)
opCheckLockTimeVerify = aop OP_CHECKLOCKTIMEVERIFY

opCheckSequenceVerify :: Fn (s :> TNat) (s :> TNat)
opCheckSequenceVerify = aop OP_CHECKSEQUENCEVERIFY

opInputIndex :: Fn s (s :> TNat)
opInputIndex = aop OP_INPUTINDEX

opActiveBytecode :: Fn s (s :> TBytes)
opActiveBytecode = aop OP_ACTIVEBYTECODE

opTxVersion :: Fn s (s :> TNat)
opTxVersion = aop OP_TXVERSION

opTxInputCount :: Fn s (s :> TNat)
opTxInputCount = aop OP_TXINPUTCOUNT

opTxOutputCount :: Fn s (s :> TNat)
opTxOutputCount = aop OP_TXOUTPUTCOUNT

opTxLockTime :: Fn s (s :> TNat)
opTxLockTime = aop OP_TXLOCKTIME

opUtxoValue :: Fn (s :> TNat) (s :> TNat)
opUtxoValue = aop OP_UTXOVALUE

opUtxoBytecode :: Fn (s :> TNat) (s :> TBytes)
opUtxoBytecode = aop OP_UTXOBYTECODE

opUtxoTokenCategory :: Fn (s :> TNat) (s :> TBytes)
opUtxoTokenCategory = aop OP_UTXOTOKENCATEGORY

opUtxoTokenCommitment :: Fn (s :> TNat) (s :> TBytes)
opUtxoTokenCommitment = aop OP_UTXOTOKENCOMMITMENT

opUtxoTokenAmount :: Fn (s :> TNat) (s :> TNat)
opUtxoTokenAmount = aop OP_UTXOTOKENAMOUNT

opOutPointTxHash :: Fn (s :> TNat) (s :> THash256)
opOutPointTxHash = aop OP_OUTPOINTTXHASH

opOutPointIndex :: Fn (s :> TNat) (s :> TNat)
opOutPointIndex = aop OP_OUTPOINTINDEX

opInputBytecode :: Fn (s :> TNat) (s :> TBytes)
opInputBytecode = aop OP_INPUTBYTECODE

opInputSequenceNumber :: Fn (s :> TNat) (s :> TNat)
opInputSequenceNumber = aop OP_INPUTSEQUENCENUMBER

opOutputValue :: Fn (s :> TNat) (s :> TNat)
opOutputValue = aop OP_OUTPUTVALUE

opOutputBytecode :: Fn (s :> TNat) (s :> TBytes)
opOutputBytecode = aop OP_OUTPUTBYTECODE

opOutputTokenCategory :: Fn (s :> TNat) (s :> TBytes)
opOutputTokenCategory = aop OP_OUTPUTTOKENCATEGORY

opOutputTokenCommitment :: Fn (s :> TNat) (s :> TBytes)
opOutputTokenCommitment = aop OP_OUTPUTTOKENCOMMITMENT

opOutputTokenAmount :: Fn (s :> TNat) (s :> TNat)
opOutputTokenAmount = aop OP_OUTPUTTOKENAMOUNT
