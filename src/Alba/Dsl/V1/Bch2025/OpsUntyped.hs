-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.OpsUntyped where

import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.StackUntyped (FNU, SU (SU))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

opTrue :: FNU
opTrue (SU c fs) = SU (aop c OP_1) fs

opFalse :: FNU
opFalse (SU c fs) = SU (aop c OP_0) fs

op0 :: FNU
op0 (SU c fs) = SU (aop c OP_0) fs

op1 :: FNU
op1 (SU c fs) = SU (aop c OP_1) fs

op2 :: FNU
op2 (SU c fs) = SU (aop c OP_2) fs

op3 :: FNU
op3 (SU c fs) = SU (aop c OP_3) fs

op4 :: FNU
op4 (SU c fs) = SU (aop c OP_4) fs

op5 :: FNU
op5 (SU c fs) = SU (aop c OP_5) fs

op6 :: FNU
op6 (SU c fs) = SU (aop c OP_6) fs

op7 :: FNU
op7 (SU c fs) = SU (aop c OP_7) fs

op8 :: FNU
op8 (SU c fs) = SU (aop c OP_8) fs

op9 :: FNU
op9 (SU c fs) = SU (aop c OP_9) fs

op10 :: FNU
op10 (SU c fs) = SU (aop c OP_10) fs

op11 :: FNU
op11 (SU c fs) = SU (aop c OP_11) fs

op12 :: FNU
op12 (SU c fs) = SU (aop c OP_12) fs

op13 :: FNU
op13 (SU c fs) = SU (aop c OP_13) fs

op14 :: FNU
op14 (SU c fs) = SU (aop c OP_14) fs

op15 :: FNU
op15 (SU c fs) = SU (aop c OP_15) fs

op16 :: FNU
op16 (SU c fs) = SU (aop c OP_16) fs

op1Negate :: FNU
op1Negate (SU c fs) = SU (aop c OP_1NEGATE) fs

opNop :: FNU
opNop (SU c fs) = SU (aop c OP_NOP) fs

opIf :: FNU -> FNU -> FNU
opIf ifOps elseOps (SU c fs) =
  let (SU c' fs') = ifOps (SU (aop c OP_IF) fs)
      (SU c'' fs'') = elseOps (SU (aop c' OP_ELSE) fs')
   in SU (aop c'' OP_ENDIF) fs''

-- Version of opIf without the else clause.
opWhen :: FNU -> FNU
opWhen body (SU c fs) =
  let (SU c' fs') = body (SU (aop c OP_IF) fs)
   in SU (aop c' OP_ENDIF) fs'

opNotIf :: FNU -> FNU -> FNU
opNotIf ifOps elseOps (SU c fs) =
  let (SU c' fs') = ifOps (SU (aop c OP_NOTIF) fs)
      (SU c'' fs'') = elseOps (SU (aop c' OP_ELSE) fs')
   in SU (aop c'' OP_ENDIF) fs''

-- Version of opNotIf without the else clause.
opUnless :: FNU -> FNU
opUnless body (SU c fs) =
  let (SU c' fs') = body (SU (aop c OP_NOTIF) fs)
   in SU (aop c' OP_ENDIF) fs'

opVerify :: FNU
opVerify (SU c fs) = SU (aop c OP_VERIFY) fs

opReturn :: FNU
opReturn (SU c fs) = SU (aop c OP_RETURN) fs

opToAltStack :: FNU
opToAltStack (SU c fs) = SU (aop c OP_TOALTSTACK) fs

opFromAltStack :: FNU
opFromAltStack (SU c fs) = SU (aop c OP_FROMALTSTACK) fs

opDepth :: FNU
opDepth (SU c fs) = SU (aop c OP_DEPTH) fs

opDrop :: FNU
opDrop (SU c fs) = SU (aop c OP_DROP) fs

opDup :: FNU
opDup (SU c fs) = SU (aop c OP_DUP) fs

opNip :: FNU
opNip (SU c fs) = SU (aop c OP_NIP) fs

opOver :: FNU
opOver (SU c fs) = SU (aop c OP_OVER) fs

opPick :: FNU
opPick (SU c fs) = SU (aop c OP_PICK) fs

opRoll :: FNU
opRoll (SU c fs) = SU (aop c OP_ROLL) fs

opRot :: FNU
opRot (SU c fs) = SU (aop c OP_ROT) fs

opSwap :: FNU
opSwap (SU c fs) = SU (aop c OP_SWAP) fs

opTuck :: FNU
opTuck (SU c fs) = SU (aop c OP_TUCK) fs

op2Drop :: FNU
op2Drop (SU c fs) = SU (aop c OP_2DROP) fs

op2Dup :: FNU
op2Dup (SU c fs) = SU (aop c OP_2DUP) fs

op3Dup :: FNU
op3Dup (SU c fs) = SU (aop c OP_3DUP) fs

op2Over :: FNU
op2Over (SU c fs) = SU (aop c OP_2OVER) fs

op2Rot :: FNU
op2Rot (SU c fs) = SU (aop c OP_2ROT) fs

op2Swap :: FNU
op2Swap (SU c fs) = SU (aop c OP_2SWAP) fs

opIfDup :: FNU
opIfDup (SU c fs) = SU (aop c OP_IFDUP) fs

opCat :: FNU
opCat (SU c fs) = SU (aop c OP_CAT) fs

opSplit :: FNU
opSplit (SU c fs) = SU (aop c OP_SPLIT) fs

opNum2Bin :: FNU
opNum2Bin (SU c fs) = SU (aop c OP_NUM2BIN) fs

opBin2Num :: FNU
opBin2Num (SU c fs) = SU (aop c OP_BIN2NUM) fs

opSize :: FNU
opSize (SU c fs) = SU (aop c OP_SIZE) fs

opAnd :: FNU
opAnd (SU c fs) = SU (aop c OP_AND) fs

opOr :: FNU
opOr (SU c fs) = SU (aop c OP_OR) fs

opXor :: FNU
opXor (SU c fs) = SU (aop c OP_XOR) fs

opReverseBytes :: FNU
opReverseBytes (SU c fs) = SU (aop c OP_REVERSEBYTES) fs

opEqual :: FNU
opEqual (SU c fs) = SU (aop c OP_EQUAL) fs

opEqualVerify :: FNU
opEqualVerify (SU c fs) = SU (aop c OP_EQUALVERIFY) fs

op1Add :: FNU
op1Add (SU c fs) = SU (aop c OP_1ADD) fs

op1Sub :: FNU
op1Sub (SU c fs) = SU (aop c OP_1SUB) fs

op1SubUnsafe :: FNU
op1SubUnsafe (SU c fs) = SU (aop c OP_1SUB) fs

opNegate :: FNU
opNegate (SU c fs) = SU (aop c OP_NEGATE) fs

opAbs :: FNU
opAbs (SU c fs) = SU (aop c OP_ABS) fs

opNot :: FNU
opNot (SU c fs) = SU (aop c OP_NOT) fs

op0NotEqual :: FNU
op0NotEqual (SU c fs) = SU (aop c OP_0NOTEQUAL) fs

opAdd :: FNU
opAdd (SU c fs) = SU (aop c OP_ADD) fs

opSub :: FNU
opSub (SU c fs) = SU (aop c OP_SUB) fs

opSubUnsafe :: FNU
opSubUnsafe (SU c fs) = SU (aop c OP_SUB) fs

opMul :: FNU
opMul (SU c fs) = SU (aop c OP_MUL) fs

opDiv :: FNU
opDiv (SU c fs) = SU (aop c OP_DIV) fs

opMod :: FNU
opMod (SU c fs) = SU (aop c OP_MOD) fs

opBoolAnd :: FNU
opBoolAnd (SU c fs) = SU (aop c OP_BOOLAND) fs

opBoolOr :: FNU
opBoolOr (SU c fs) = SU (aop c OP_BOOLOR) fs

opNumEqual :: FNU
opNumEqual (SU c fs) = SU (aop c OP_NUMEQUAL) fs

opNumEqualVerify :: FNU
opNumEqualVerify (SU c fs) = SU (aop c OP_NUMEQUALVERIFY) fs

opNumNotEqual :: FNU
opNumNotEqual (SU c fs) = SU (aop c OP_NUMNOTEQUAL) fs

opLessThan :: FNU
opLessThan (SU c fs) = SU (aop c OP_LESSTHAN) fs

opGreaterThan :: FNU
opGreaterThan (SU c fs) = SU (aop c OP_GREATERTHAN) fs

opLessThanOrEqual :: FNU
opLessThanOrEqual (SU c fs) = SU (aop c OP_LESSTHANOREQUAL) fs

opGreaterThanOrEqual :: FNU
opGreaterThanOrEqual (SU c fs) = SU (aop c OP_GREATERTHANOREQUAL) fs

opMin :: FNU
opMin (SU c fs) = SU (aop c OP_MIN) fs

opMax :: FNU
opMax (SU c fs) = SU (aop c OP_MAX) fs

opWithin :: FNU
opWithin (SU c fs) = SU (aop c OP_WITHIN) fs

opRipemd160 :: FNU
opRipemd160 (SU c fs) = SU (aop c OP_RIPEMD160) fs

opSha1 :: FNU
opSha1 (SU c fs) = SU (aop c OP_SHA1) fs

opSha256 :: FNU
opSha256 (SU c fs) = SU (aop c OP_SHA256) fs

opHash160 :: FNU
opHash160 (SU c fs) = SU (aop c OP_HASH160) fs

opHash256 :: FNU
opHash256 (SU c fs) = SU (aop c OP_HASH256) fs

opCodeSeparator :: FNU
opCodeSeparator (SU c fs) = SU (aop c OP_CODESEPARATOR) fs

opCheckSig :: FNU
opCheckSig (SU c fs) = SU (aop c OP_CHECKSIG) fs

opCheckSigVerify :: FNU
opCheckSigVerify (SU c fs) = SU (aop c OP_CHECKSIGVERIFY) fs

opCheckMultiSig :: FNU
opCheckMultiSig (SU c fs) = SU (aop c OP_CHECKMULTISIG) fs

opCheckMultiSigVerify :: FNU
opCheckMultiSigVerify (SU c fs) = SU (aop c OP_CHECKMULTISIGVERIFY) fs

opCheckDataSig :: FNU
opCheckDataSig (SU c fs) = SU (aop c OP_CHECKDATASIG) fs

opCheckDataSigVerify :: FNU
opCheckDataSigVerify (SU c fs) = SU (aop c OP_CHECKDATASIGVERIFY) fs

opCheckLockTimeVerify :: FNU
opCheckLockTimeVerify (SU c fs) = SU (aop c OP_CHECKLOCKTIMEVERIFY) fs

opCheckSequenceVerify :: FNU
opCheckSequenceVerify (SU c fs) = SU (aop c OP_CHECKSEQUENCEVERIFY) fs

opInputIndex :: FNU
opInputIndex (SU c fs) = SU (aop c OP_INPUTINDEX) fs

opActiveBytecode :: FNU
opActiveBytecode (SU c fs) = SU (aop c OP_ACTIVEBYTECODE) fs

opTxVersion :: FNU
opTxVersion (SU c fs) = SU (aop c OP_TXVERSION) fs

opTxInputCount :: FNU
opTxInputCount (SU c fs) = SU (aop c OP_TXINPUTCOUNT) fs

opTxOutputCount :: FNU
opTxOutputCount (SU c fs) = SU (aop c OP_TXOUTPUTCOUNT) fs

opTxLockTime :: FNU
opTxLockTime (SU c fs) = SU (aop c OP_TXLOCKTIME) fs

opUtxoValue :: FNU
opUtxoValue (SU c fs) = SU (aop c OP_UTXOVALUE) fs

opUtxoBytecode :: FNU
opUtxoBytecode (SU c fs) = SU (aop c OP_UTXOBYTECODE) fs

opUtxoTokenCategory :: FNU
opUtxoTokenCategory (SU c fs) = SU (aop c OP_UTXOTOKENCATEGORY) fs

opUtxoTokenCommitment :: FNU
opUtxoTokenCommitment (SU c fs) = SU (aop c OP_UTXOTOKENCOMMITMENT) fs

opUtxoTokenAmount :: FNU
opUtxoTokenAmount (SU c fs) = SU (aop c OP_UTXOTOKENAMOUNT) fs

opOutPointTxHash :: FNU
opOutPointTxHash (SU c fs) = SU (aop c OP_OUTPOINTTXHASH) fs

opOutPointIndex :: FNU
opOutPointIndex (SU c fs) = SU (aop c OP_OUTPOINTINDEX) fs

opInputBytecode :: FNU
opInputBytecode (SU c fs) = SU (aop c OP_INPUTBYTECODE) fs

opInputSequenceNumber :: FNU
opInputSequenceNumber (SU c fs) = SU (aop c OP_INPUTSEQUENCENUMBER) fs

opOutputValue :: FNU
opOutputValue (SU c fs) = SU (aop c OP_OUTPUTVALUE) fs

opOutputBytecode :: FNU
opOutputBytecode (SU c fs) = SU (aop c OP_OUTPUTBYTECODE) fs

opOutputTokenCategory :: FNU
opOutputTokenCategory (SU c fs) = SU (aop c OP_OUTPUTTOKENCATEGORY) fs

opOutputTokenCommitment :: FNU
opOutputTokenCommitment (SU c fs) = SU (aop c OP_OUTPUTTOKENCOMMITMENT) fs

opOutputTokenAmount :: FNU
opOutputTokenAmount (SU c fs) = SU (aop c OP_OUTPUTTOKENAMOUNT) fs
