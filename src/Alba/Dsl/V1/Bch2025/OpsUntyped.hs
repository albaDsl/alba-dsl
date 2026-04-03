-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.OpsUntyped where

import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.StackUntyped (FnU, SU (SU))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

opTrue :: FnU
opTrue (SU c fs) = SU (aop c OP_1) fs

opFalse :: FnU
opFalse (SU c fs) = SU (aop c OP_0) fs

op0 :: FnU
op0 (SU c fs) = SU (aop c OP_0) fs

op1 :: FnU
op1 (SU c fs) = SU (aop c OP_1) fs

op2 :: FnU
op2 (SU c fs) = SU (aop c OP_2) fs

op3 :: FnU
op3 (SU c fs) = SU (aop c OP_3) fs

op4 :: FnU
op4 (SU c fs) = SU (aop c OP_4) fs

op5 :: FnU
op5 (SU c fs) = SU (aop c OP_5) fs

op6 :: FnU
op6 (SU c fs) = SU (aop c OP_6) fs

op7 :: FnU
op7 (SU c fs) = SU (aop c OP_7) fs

op8 :: FnU
op8 (SU c fs) = SU (aop c OP_8) fs

op9 :: FnU
op9 (SU c fs) = SU (aop c OP_9) fs

op10 :: FnU
op10 (SU c fs) = SU (aop c OP_10) fs

op11 :: FnU
op11 (SU c fs) = SU (aop c OP_11) fs

op12 :: FnU
op12 (SU c fs) = SU (aop c OP_12) fs

op13 :: FnU
op13 (SU c fs) = SU (aop c OP_13) fs

op14 :: FnU
op14 (SU c fs) = SU (aop c OP_14) fs

op15 :: FnU
op15 (SU c fs) = SU (aop c OP_15) fs

op16 :: FnU
op16 (SU c fs) = SU (aop c OP_16) fs

op1Negate :: FnU
op1Negate (SU c fs) = SU (aop c OP_1NEGATE) fs

opNop :: FnU
opNop (SU c fs) = SU (aop c OP_NOP) fs

opIf :: FnU -> FnU -> FnU
opIf ifOps elseOps (SU c fs) =
  let (SU c' fs') = ifOps (SU (aop c OP_IF) fs)
      (SU c'' fs'') = elseOps (SU (aop c' OP_ELSE) fs')
   in SU (aop c'' OP_ENDIF) fs''

-- Version of opIf without the else clause.
opWhen :: FnU -> FnU
opWhen body (SU c fs) =
  let (SU c' fs') = body (SU (aop c OP_IF) fs)
   in SU (aop c' OP_ENDIF) fs'

opNotIf :: FnU -> FnU -> FnU
opNotIf ifOps elseOps (SU c fs) =
  let (SU c' fs') = ifOps (SU (aop c OP_NOTIF) fs)
      (SU c'' fs'') = elseOps (SU (aop c' OP_ELSE) fs')
   in SU (aop c'' OP_ENDIF) fs''

-- Version of opNotIf without the else clause.
opUnless :: FnU -> FnU
opUnless body (SU c fs) =
  let (SU c' fs') = body (SU (aop c OP_NOTIF) fs)
   in SU (aop c' OP_ENDIF) fs'

opVerify :: FnU
opVerify (SU c fs) = SU (aop c OP_VERIFY) fs

opReturn :: FnU
opReturn (SU c fs) = SU (aop c OP_RETURN) fs

opToAltStack :: FnU
opToAltStack (SU c fs) = SU (aop c OP_TOALTSTACK) fs

opFromAltStack :: FnU
opFromAltStack (SU c fs) = SU (aop c OP_FROMALTSTACK) fs

opDepth :: FnU
opDepth (SU c fs) = SU (aop c OP_DEPTH) fs

opDrop :: FnU
opDrop (SU c fs) = SU (aop c OP_DROP) fs

opDup :: FnU
opDup (SU c fs) = SU (aop c OP_DUP) fs

opNip :: FnU
opNip (SU c fs) = SU (aop c OP_NIP) fs

opOver :: FnU
opOver (SU c fs) = SU (aop c OP_OVER) fs

opPick :: FnU
opPick (SU c fs) = SU (aop c OP_PICK) fs

opRoll :: FnU
opRoll (SU c fs) = SU (aop c OP_ROLL) fs

opRot :: FnU
opRot (SU c fs) = SU (aop c OP_ROT) fs

opSwap :: FnU
opSwap (SU c fs) = SU (aop c OP_SWAP) fs

opTuck :: FnU
opTuck (SU c fs) = SU (aop c OP_TUCK) fs

op2Drop :: FnU
op2Drop (SU c fs) = SU (aop c OP_2DROP) fs

op2Dup :: FnU
op2Dup (SU c fs) = SU (aop c OP_2DUP) fs

op3Dup :: FnU
op3Dup (SU c fs) = SU (aop c OP_3DUP) fs

op2Over :: FnU
op2Over (SU c fs) = SU (aop c OP_2OVER) fs

op2Rot :: FnU
op2Rot (SU c fs) = SU (aop c OP_2ROT) fs

op2Swap :: FnU
op2Swap (SU c fs) = SU (aop c OP_2SWAP) fs

opIfDup :: FnU
opIfDup (SU c fs) = SU (aop c OP_IFDUP) fs

opCat :: FnU
opCat (SU c fs) = SU (aop c OP_CAT) fs

opSplit :: FnU
opSplit (SU c fs) = SU (aop c OP_SPLIT) fs

opNum2Bin :: FnU
opNum2Bin (SU c fs) = SU (aop c OP_NUM2BIN) fs

opBin2Num :: FnU
opBin2Num (SU c fs) = SU (aop c OP_BIN2NUM) fs

opSize :: FnU
opSize (SU c fs) = SU (aop c OP_SIZE) fs

opAnd :: FnU
opAnd (SU c fs) = SU (aop c OP_AND) fs

opOr :: FnU
opOr (SU c fs) = SU (aop c OP_OR) fs

opXor :: FnU
opXor (SU c fs) = SU (aop c OP_XOR) fs

opReverseBytes :: FnU
opReverseBytes (SU c fs) = SU (aop c OP_REVERSEBYTES) fs

opEqual :: FnU
opEqual (SU c fs) = SU (aop c OP_EQUAL) fs

opEqualVerify :: FnU
opEqualVerify (SU c fs) = SU (aop c OP_EQUALVERIFY) fs

op1Add :: FnU
op1Add (SU c fs) = SU (aop c OP_1ADD) fs

op1Sub :: FnU
op1Sub (SU c fs) = SU (aop c OP_1SUB) fs

op1SubUnsafe :: FnU
op1SubUnsafe (SU c fs) = SU (aop c OP_1SUB) fs

opNegate :: FnU
opNegate (SU c fs) = SU (aop c OP_NEGATE) fs

opAbs :: FnU
opAbs (SU c fs) = SU (aop c OP_ABS) fs

opNot :: FnU
opNot (SU c fs) = SU (aop c OP_NOT) fs

op0NotEqual :: FnU
op0NotEqual (SU c fs) = SU (aop c OP_0NOTEQUAL) fs

opAdd :: FnU
opAdd (SU c fs) = SU (aop c OP_ADD) fs

opSub :: FnU
opSub (SU c fs) = SU (aop c OP_SUB) fs

opSubUnsafe :: FnU
opSubUnsafe (SU c fs) = SU (aop c OP_SUB) fs

opMul :: FnU
opMul (SU c fs) = SU (aop c OP_MUL) fs

opDiv :: FnU
opDiv (SU c fs) = SU (aop c OP_DIV) fs

opMod :: FnU
opMod (SU c fs) = SU (aop c OP_MOD) fs

opBoolAnd :: FnU
opBoolAnd (SU c fs) = SU (aop c OP_BOOLAND) fs

opBoolOr :: FnU
opBoolOr (SU c fs) = SU (aop c OP_BOOLOR) fs

opNumEqual :: FnU
opNumEqual (SU c fs) = SU (aop c OP_NUMEQUAL) fs

opNumEqualVerify :: FnU
opNumEqualVerify (SU c fs) = SU (aop c OP_NUMEQUALVERIFY) fs

opNumNotEqual :: FnU
opNumNotEqual (SU c fs) = SU (aop c OP_NUMNOTEQUAL) fs

opLessThan :: FnU
opLessThan (SU c fs) = SU (aop c OP_LESSTHAN) fs

opGreaterThan :: FnU
opGreaterThan (SU c fs) = SU (aop c OP_GREATERTHAN) fs

opLessThanOrEqual :: FnU
opLessThanOrEqual (SU c fs) = SU (aop c OP_LESSTHANOREQUAL) fs

opGreaterThanOrEqual :: FnU
opGreaterThanOrEqual (SU c fs) = SU (aop c OP_GREATERTHANOREQUAL) fs

opMin :: FnU
opMin (SU c fs) = SU (aop c OP_MIN) fs

opMax :: FnU
opMax (SU c fs) = SU (aop c OP_MAX) fs

opWithin :: FnU
opWithin (SU c fs) = SU (aop c OP_WITHIN) fs

opRipemd160 :: FnU
opRipemd160 (SU c fs) = SU (aop c OP_RIPEMD160) fs

opSha1 :: FnU
opSha1 (SU c fs) = SU (aop c OP_SHA1) fs

opSha256 :: FnU
opSha256 (SU c fs) = SU (aop c OP_SHA256) fs

opHash160 :: FnU
opHash160 (SU c fs) = SU (aop c OP_HASH160) fs

opHash256 :: FnU
opHash256 (SU c fs) = SU (aop c OP_HASH256) fs

opCodeSeparator :: FnU
opCodeSeparator (SU c fs) = SU (aop c OP_CODESEPARATOR) fs

opCheckSig :: FnU
opCheckSig (SU c fs) = SU (aop c OP_CHECKSIG) fs

opCheckSigVerify :: FnU
opCheckSigVerify (SU c fs) = SU (aop c OP_CHECKSIGVERIFY) fs

opCheckMultiSig :: FnU
opCheckMultiSig (SU c fs) = SU (aop c OP_CHECKMULTISIG) fs

opCheckMultiSigVerify :: FnU
opCheckMultiSigVerify (SU c fs) = SU (aop c OP_CHECKMULTISIGVERIFY) fs

opCheckDataSig :: FnU
opCheckDataSig (SU c fs) = SU (aop c OP_CHECKDATASIG) fs

opCheckDataSigVerify :: FnU
opCheckDataSigVerify (SU c fs) = SU (aop c OP_CHECKDATASIGVERIFY) fs

opCheckLockTimeVerify :: FnU
opCheckLockTimeVerify (SU c fs) = SU (aop c OP_CHECKLOCKTIMEVERIFY) fs

opCheckSequenceVerify :: FnU
opCheckSequenceVerify (SU c fs) = SU (aop c OP_CHECKSEQUENCEVERIFY) fs

opInputIndex :: FnU
opInputIndex (SU c fs) = SU (aop c OP_INPUTINDEX) fs

opActiveBytecode :: FnU
opActiveBytecode (SU c fs) = SU (aop c OP_ACTIVEBYTECODE) fs

opTxVersion :: FnU
opTxVersion (SU c fs) = SU (aop c OP_TXVERSION) fs

opTxInputCount :: FnU
opTxInputCount (SU c fs) = SU (aop c OP_TXINPUTCOUNT) fs

opTxOutputCount :: FnU
opTxOutputCount (SU c fs) = SU (aop c OP_TXOUTPUTCOUNT) fs

opTxLockTime :: FnU
opTxLockTime (SU c fs) = SU (aop c OP_TXLOCKTIME) fs

opUtxoValue :: FnU
opUtxoValue (SU c fs) = SU (aop c OP_UTXOVALUE) fs

opUtxoBytecode :: FnU
opUtxoBytecode (SU c fs) = SU (aop c OP_UTXOBYTECODE) fs

opUtxoTokenCategory :: FnU
opUtxoTokenCategory (SU c fs) = SU (aop c OP_UTXOTOKENCATEGORY) fs

opUtxoTokenCommitment :: FnU
opUtxoTokenCommitment (SU c fs) = SU (aop c OP_UTXOTOKENCOMMITMENT) fs

opUtxoTokenAmount :: FnU
opUtxoTokenAmount (SU c fs) = SU (aop c OP_UTXOTOKENAMOUNT) fs

opOutPointTxHash :: FnU
opOutPointTxHash (SU c fs) = SU (aop c OP_OUTPOINTTXHASH) fs

opOutPointIndex :: FnU
opOutPointIndex (SU c fs) = SU (aop c OP_OUTPOINTINDEX) fs

opInputBytecode :: FnU
opInputBytecode (SU c fs) = SU (aop c OP_INPUTBYTECODE) fs

opInputSequenceNumber :: FnU
opInputSequenceNumber (SU c fs) = SU (aop c OP_INPUTSEQUENCENUMBER) fs

opOutputValue :: FnU
opOutputValue (SU c fs) = SU (aop c OP_OUTPUTVALUE) fs

opOutputBytecode :: FnU
opOutputBytecode (SU c fs) = SU (aop c OP_OUTPUTBYTECODE) fs

opOutputTokenCategory :: FnU
opOutputTokenCategory (SU c fs) = SU (aop c OP_OUTPUTTOKENCATEGORY) fs

opOutputTokenCommitment :: FnU
opOutputTokenCommitment (SU c fs) = SU (aop c OP_OUTPUTTOKENCOMMITMENT) fs

opOutputTokenAmount :: FnU
opOutputTokenAmount (SU c fs) = SU (aop c OP_OUTPUTTOKENAMOUNT) fs
