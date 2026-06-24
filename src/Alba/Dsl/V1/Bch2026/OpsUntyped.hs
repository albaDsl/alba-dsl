-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.OpsUntyped where

import Alba.Dsl.V1.Common.CompilerUtilsUntyped (aop)
import Alba.Dsl.V1.Common.StackUntyped (FnU)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Control.Arrow ((>>>))

opTrue :: FnU
opTrue = aop OP_1

opFalse :: FnU
opFalse = aop OP_0

op0 :: FnU
op0 = aop OP_0

op1 :: FnU
op1 = aop OP_1

op2 :: FnU
op2 = aop OP_2

op3 :: FnU
op3 = aop OP_3

op4 :: FnU
op4 = aop OP_4

op5 :: FnU
op5 = aop OP_5

op6 :: FnU
op6 = aop OP_6

op7 :: FnU
op7 = aop OP_7

op8 :: FnU
op8 = aop OP_8

op9 :: FnU
op9 = aop OP_9

op10 :: FnU
op10 = aop OP_10

op11 :: FnU
op11 = aop OP_11

op12 :: FnU
op12 = aop OP_12

op13 :: FnU
op13 = aop OP_13

op14 :: FnU
op14 = aop OP_14

op15 :: FnU
op15 = aop OP_15

op16 :: FnU
op16 = aop OP_16

op1Negate :: FnU
op1Negate = aop OP_1NEGATE

opNop :: FnU
opNop = aop OP_NOP

opIf :: FnU -> FnU -> FnU
opIf ifOps elseOps =
  aop OP_IF >>> ifOps >>> aop OP_ELSE >>> elseOps >>> aop OP_ENDIF

-- Version of opIf without the else clause.
opWhen :: FnU -> FnU
opWhen body = aop OP_IF >>> body >>> aop OP_ENDIF

opNotIf :: FnU -> FnU -> FnU
opNotIf ifOps elseOps =
  aop OP_NOTIF >>> ifOps >>> aop OP_ELSE >>> elseOps >>> aop OP_ENDIF

-- Version of opNotIf without the else clause.
opUnless :: FnU -> FnU
opUnless body = aop OP_NOTIF >>> body >>> aop OP_ENDIF

opVerify :: FnU
opVerify = aop OP_VERIFY

opReturn :: FnU
opReturn = aop OP_RETURN

opToAltStack :: FnU
opToAltStack = aop OP_TOALTSTACK

opFromAltStack :: FnU
opFromAltStack = aop OP_FROMALTSTACK

opDepth :: FnU
opDepth = aop OP_DEPTH

opDrop :: FnU
opDrop = aop OP_DROP

opDup :: FnU
opDup = aop OP_DUP

opNip :: FnU
opNip = aop OP_NIP

opOver :: FnU
opOver = aop OP_OVER

opPick :: FnU
opPick = aop OP_PICK

opRoll :: FnU
opRoll = aop OP_ROLL

opRot :: FnU
opRot = aop OP_ROT

opSwap :: FnU
opSwap = aop OP_SWAP

opTuck :: FnU
opTuck = aop OP_TUCK

op2Drop :: FnU
op2Drop = aop OP_2DROP

op2Dup :: FnU
op2Dup = aop OP_2DUP

op3Dup :: FnU
op3Dup = aop OP_3DUP

op2Over :: FnU
op2Over = aop OP_2OVER

op2Rot :: FnU
op2Rot = aop OP_2ROT

op2Swap :: FnU
op2Swap = aop OP_2SWAP

opIfDup :: FnU
opIfDup = aop OP_IFDUP

opCat :: FnU
opCat = aop OP_CAT

opSplit :: FnU
opSplit = aop OP_SPLIT

opNum2Bin :: FnU
opNum2Bin = aop OP_NUM2BIN

opBin2Num :: FnU
opBin2Num = aop OP_BIN2NUM

opSize :: FnU
opSize = aop OP_SIZE

opAnd :: FnU
opAnd = aop OP_AND

opOr :: FnU
opOr = aop OP_OR

opXor :: FnU
opXor = aop OP_XOR

opReverseBytes :: FnU
opReverseBytes = aop OP_REVERSEBYTES

opEqual :: FnU
opEqual = aop OP_EQUAL

opEqualVerify :: FnU
opEqualVerify = aop OP_EQUALVERIFY

op1Add :: FnU
op1Add = aop OP_1ADD

op1Sub :: FnU
op1Sub = aop OP_1SUB

opNegate :: FnU
opNegate = aop OP_NEGATE

opAbs :: FnU
opAbs = aop OP_ABS

opNot :: FnU
opNot = aop OP_NOT

op0NotEqual :: FnU
op0NotEqual = aop OP_0NOTEQUAL

opAdd :: FnU
opAdd = aop OP_ADD

opSub :: FnU
opSub = aop OP_SUB

opMul :: FnU
opMul = aop OP_MUL

opDiv :: FnU
opDiv = aop OP_DIV

opMod :: FnU
opMod = aop OP_MOD

opBoolAnd :: FnU
opBoolAnd = aop OP_BOOLAND

opBoolOr :: FnU
opBoolOr = aop OP_BOOLOR

opNumEqual :: FnU
opNumEqual = aop OP_NUMEQUAL

opNumEqualVerify :: FnU
opNumEqualVerify = aop OP_NUMEQUALVERIFY

opNumNotEqual :: FnU
opNumNotEqual = aop OP_NUMNOTEQUAL

opLessThan :: FnU
opLessThan = aop OP_LESSTHAN

opGreaterThan :: FnU
opGreaterThan = aop OP_GREATERTHAN

opLessThanOrEqual :: FnU
opLessThanOrEqual = aop OP_LESSTHANOREQUAL

opGreaterThanOrEqual :: FnU
opGreaterThanOrEqual = aop OP_GREATERTHANOREQUAL

opMin :: FnU
opMin = aop OP_MIN

opMax :: FnU
opMax = aop OP_MAX

opWithin :: FnU
opWithin = aop OP_WITHIN

opRipemd160 :: FnU
opRipemd160 = aop OP_RIPEMD160

opSha1 :: FnU
opSha1 = aop OP_SHA1

opSha256 :: FnU
opSha256 = aop OP_SHA256

opHash160 :: FnU
opHash160 = aop OP_HASH160

opHash256 :: FnU
opHash256 = aop OP_HASH256

opCodeSeparator :: FnU
opCodeSeparator = aop OP_CODESEPARATOR

opCheckSig :: FnU
opCheckSig = aop OP_CHECKSIG

opCheckSigVerify :: FnU
opCheckSigVerify = aop OP_CHECKSIGVERIFY

opCheckMultiSig :: FnU
opCheckMultiSig = aop OP_CHECKMULTISIG

opCheckMultiSigVerify :: FnU
opCheckMultiSigVerify = aop OP_CHECKMULTISIGVERIFY

opCheckDataSig :: FnU
opCheckDataSig = aop OP_CHECKDATASIG

opCheckDataSigVerify :: FnU
opCheckDataSigVerify = aop OP_CHECKDATASIGVERIFY

opCheckLockTimeVerify :: FnU
opCheckLockTimeVerify = aop OP_CHECKLOCKTIMEVERIFY

opCheckSequenceVerify :: FnU
opCheckSequenceVerify = aop OP_CHECKSEQUENCEVERIFY

opInputIndex :: FnU
opInputIndex = aop OP_INPUTINDEX

opActiveBytecode :: FnU
opActiveBytecode = aop OP_ACTIVEBYTECODE

opTxVersion :: FnU
opTxVersion = aop OP_TXVERSION

opTxInputCount :: FnU
opTxInputCount = aop OP_TXINPUTCOUNT

opTxOutputCount :: FnU
opTxOutputCount = aop OP_TXOUTPUTCOUNT

opTxLockTime :: FnU
opTxLockTime = aop OP_TXLOCKTIME

opUtxoValue :: FnU
opUtxoValue = aop OP_UTXOVALUE

opUtxoBytecode :: FnU
opUtxoBytecode = aop OP_UTXOBYTECODE

opUtxoTokenCategory :: FnU
opUtxoTokenCategory = aop OP_UTXOTOKENCATEGORY

opUtxoTokenCommitment :: FnU
opUtxoTokenCommitment = aop OP_UTXOTOKENCOMMITMENT

opUtxoTokenAmount :: FnU
opUtxoTokenAmount = aop OP_UTXOTOKENAMOUNT

opOutPointTxHash :: FnU
opOutPointTxHash = aop OP_OUTPOINTTXHASH

opOutPointIndex :: FnU
opOutPointIndex = aop OP_OUTPOINTINDEX

opInputBytecode :: FnU
opInputBytecode = aop OP_INPUTBYTECODE

opInputSequenceNumber :: FnU
opInputSequenceNumber = aop OP_INPUTSEQUENCENUMBER

opOutputValue :: FnU
opOutputValue = aop OP_OUTPUTVALUE

opOutputBytecode :: FnU
opOutputBytecode = aop OP_OUTPUTBYTECODE

opOutputTokenCategory :: FnU
opOutputTokenCategory = aop OP_OUTPUTTOKENCATEGORY

opOutputTokenCommitment :: FnU
opOutputTokenCommitment = aop OP_OUTPUTTOKENCOMMITMENT

opOutputTokenAmount :: FnU
opOutputTokenAmount = aop OP_OUTPUTTOKENAMOUNT

opUntil :: FnU -> FnU
opUntil loopBody = aop OP_BEGIN >>> loopBody >>> aop OP_UNTIL

opDefine :: FnU
opDefine = aop OP_DEFINE

opInvoke :: FnU
opInvoke = aop OP_INVOKE
