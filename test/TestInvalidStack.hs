-- Copyright (c) 2025 albaDsl

module TestInvalidStack (testInvalidStack) where

import Alba.Tx.Bch2025 (Tx (..))
import Alba.Vm.Common
  ( ScriptError (..),
    VmStack,
    b2SeUnsafe,
    boolToStackElement,
    i2SeUnsafe,
    mkTxContext,
  )
import Alba.Vm.Common.OpcodeL1 (OpcodeL1)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import QuickCheckSupport ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateScript)
import Prelude hiding (exp)

data ArgCount = Args Int | ZeroArg | PushData | Special | UnusedOp

testInvalidStack :: TestTree
testInvalidStack =
  testGroup
    "Invalid Stack"
    [ testCase "Invalid Stack" $
        mapM_
          ( \opcodeL1 -> do
              let argCount = opArgCount opcodeL1
              case argCount of
                Args argCount' ->
                  evaluateWithStack opcodeL1 (stack (pred argCount'))
                    @?= Left SeInvalidStackOperation
                _ -> pure ()
          )
          ([L1.OP_0 .. L1.OP_UNASSIGNED_FF] :: [OpcodeL1]),
      testCase "Invalid Stack — OP_FROMALTSTACK" $
        evaluateWithStack L1.OP_FROMALTSTACK S.empty
          @?= Left SeInvalidStackOperation,
      testCase "Invalid Stack — OP_CHECKMULTISIG" $
        mapM_
          ( \s -> do
              evaluateWithStack L1.OP_CHECKMULTISIG s
                @?= Left SeInvalidStackOperation
          )
          multiSigStacks,
      testCase "Invalid Stack — OP_CHECKMULTISIGVERIFY" $
        mapM_
          ( \s -> do
              evaluateWithStack L1.OP_CHECKMULTISIGVERIFY s
                @?= Left SeInvalidStackOperation
          )
          multiSigStacks
    ]
  where
    stack n = S.fromList $ replicate n (boolToStackElement False)
    txContext = fromJust $ mkTxContext tx 0 undefined
    tx = Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}

    evaluateWithStack op s =
      evaluateScript [fromIntegral $ fromEnum op] (s, S.empty) txContext

    multiSigStacks =
      [ S.empty,
        S.fromList [i2SeUnsafe 1],
        S.fromList [b2SeUnsafe B.empty, i2SeUnsafe 1],
        S.fromList [i2SeUnsafe 1, b2SeUnsafe B.empty, i2SeUnsafe 1],
        S.fromList
          [ b2SeUnsafe B.empty,
            i2SeUnsafe 1,
            b2SeUnsafe B.empty,
            i2SeUnsafe 1
          ]
      ] ::
        [VmStack]

opArgCount :: OpcodeL1 -> ArgCount
opArgCount L1.OP_0 = ZeroArg
opArgCount L1.OP_DATA_01 = PushData
opArgCount L1.OP_DATA_02 = PushData
opArgCount L1.OP_DATA_03 = PushData
opArgCount L1.OP_DATA_04 = PushData
opArgCount L1.OP_DATA_05 = PushData
opArgCount L1.OP_DATA_06 = PushData
opArgCount L1.OP_DATA_07 = PushData
opArgCount L1.OP_DATA_08 = PushData
opArgCount L1.OP_DATA_09 = PushData
opArgCount L1.OP_DATA_10 = PushData
opArgCount L1.OP_DATA_11 = PushData
opArgCount L1.OP_DATA_12 = PushData
opArgCount L1.OP_DATA_13 = PushData
opArgCount L1.OP_DATA_14 = PushData
opArgCount L1.OP_DATA_15 = PushData
opArgCount L1.OP_DATA_16 = PushData
opArgCount L1.OP_DATA_17 = PushData
opArgCount L1.OP_DATA_18 = PushData
opArgCount L1.OP_DATA_19 = PushData
opArgCount L1.OP_DATA_20 = PushData
opArgCount L1.OP_DATA_21 = PushData
opArgCount L1.OP_DATA_22 = PushData
opArgCount L1.OP_DATA_23 = PushData
opArgCount L1.OP_DATA_24 = PushData
opArgCount L1.OP_DATA_25 = PushData
opArgCount L1.OP_DATA_26 = PushData
opArgCount L1.OP_DATA_27 = PushData
opArgCount L1.OP_DATA_28 = PushData
opArgCount L1.OP_DATA_29 = PushData
opArgCount L1.OP_DATA_30 = PushData
opArgCount L1.OP_DATA_31 = PushData
opArgCount L1.OP_DATA_32 = PushData
opArgCount L1.OP_DATA_33 = PushData
opArgCount L1.OP_DATA_34 = PushData
opArgCount L1.OP_DATA_35 = PushData
opArgCount L1.OP_DATA_36 = PushData
opArgCount L1.OP_DATA_37 = PushData
opArgCount L1.OP_DATA_38 = PushData
opArgCount L1.OP_DATA_39 = PushData
opArgCount L1.OP_DATA_40 = PushData
opArgCount L1.OP_DATA_41 = PushData
opArgCount L1.OP_DATA_42 = PushData
opArgCount L1.OP_DATA_43 = PushData
opArgCount L1.OP_DATA_44 = PushData
opArgCount L1.OP_DATA_45 = PushData
opArgCount L1.OP_DATA_46 = PushData
opArgCount L1.OP_DATA_47 = PushData
opArgCount L1.OP_DATA_48 = PushData
opArgCount L1.OP_DATA_49 = PushData
opArgCount L1.OP_DATA_50 = PushData
opArgCount L1.OP_DATA_51 = PushData
opArgCount L1.OP_DATA_52 = PushData
opArgCount L1.OP_DATA_53 = PushData
opArgCount L1.OP_DATA_54 = PushData
opArgCount L1.OP_DATA_55 = PushData
opArgCount L1.OP_DATA_56 = PushData
opArgCount L1.OP_DATA_57 = PushData
opArgCount L1.OP_DATA_58 = PushData
opArgCount L1.OP_DATA_59 = PushData
opArgCount L1.OP_DATA_60 = PushData
opArgCount L1.OP_DATA_61 = PushData
opArgCount L1.OP_DATA_62 = PushData
opArgCount L1.OP_DATA_63 = PushData
opArgCount L1.OP_DATA_64 = PushData
opArgCount L1.OP_DATA_65 = PushData
opArgCount L1.OP_DATA_66 = PushData
opArgCount L1.OP_DATA_67 = PushData
opArgCount L1.OP_DATA_68 = PushData
opArgCount L1.OP_DATA_69 = PushData
opArgCount L1.OP_DATA_70 = PushData
opArgCount L1.OP_DATA_71 = PushData
opArgCount L1.OP_DATA_72 = PushData
opArgCount L1.OP_DATA_73 = PushData
opArgCount L1.OP_DATA_74 = PushData
opArgCount L1.OP_DATA_75 = PushData
opArgCount L1.OP_PUSHDATA1 = PushData
opArgCount L1.OP_PUSHDATA2 = PushData
opArgCount L1.OP_PUSHDATA4 = PushData
opArgCount L1.OP_1NEGATE = ZeroArg
opArgCount L1.OP_RESERVED = UnusedOp
opArgCount L1.OP_1 = ZeroArg
opArgCount L1.OP_2 = ZeroArg
opArgCount L1.OP_3 = ZeroArg
opArgCount L1.OP_4 = ZeroArg
opArgCount L1.OP_5 = ZeroArg
opArgCount L1.OP_6 = ZeroArg
opArgCount L1.OP_7 = ZeroArg
opArgCount L1.OP_8 = ZeroArg
opArgCount L1.OP_9 = ZeroArg
opArgCount L1.OP_10 = ZeroArg
opArgCount L1.OP_11 = ZeroArg
opArgCount L1.OP_12 = ZeroArg
opArgCount L1.OP_13 = ZeroArg
opArgCount L1.OP_14 = ZeroArg
opArgCount L1.OP_15 = ZeroArg
opArgCount L1.OP_16 = ZeroArg
opArgCount L1.OP_NOP = ZeroArg
opArgCount L1.OP_VER_OP_EVAL = UnusedOp -- FIXME: TBD, change to 1-arg.
opArgCount L1.OP_IF = Args 1
opArgCount L1.OP_NOTIF = Args 1
opArgCount L1.OP_VERIF_OP_BEGIN = UnusedOp -- FIXME: TBD, change to 1-arg.
opArgCount L1.OP_VERNOTIF_OP_UNTIL = UnusedOp -- FIXME: TBD, change to 1-arg.
opArgCount L1.OP_ELSE = ZeroArg
opArgCount L1.OP_ENDIF = ZeroArg
opArgCount L1.OP_VERIFY = Args 1
opArgCount L1.OP_RETURN = ZeroArg
opArgCount L1.OP_TOALTSTACK = Args 1
opArgCount L1.OP_FROMALTSTACK = Special
opArgCount L1.OP_2DROP = Args 2
opArgCount L1.OP_2DUP = Args 2
opArgCount L1.OP_3DUP = Args 3
opArgCount L1.OP_2OVER = Args 4
opArgCount L1.OP_2ROT = Args 6
opArgCount L1.OP_2SWAP = Args 4
opArgCount L1.OP_IFDUP = Args 1
opArgCount L1.OP_DEPTH = ZeroArg
opArgCount L1.OP_DROP = Args 1
opArgCount L1.OP_DUP = Args 1
opArgCount L1.OP_NIP = Args 2
opArgCount L1.OP_OVER = Args 2
opArgCount L1.OP_PICK = Args 2
opArgCount L1.OP_ROLL = Args 2
opArgCount L1.OP_ROT = Args 3
opArgCount L1.OP_SWAP = Args 2
opArgCount L1.OP_TUCK = Args 2
opArgCount L1.OP_CAT = Args 2
opArgCount L1.OP_SPLIT = Args 2
opArgCount L1.OP_NUM2BIN = Args 2
opArgCount L1.OP_BIN2NUM = Args 1
opArgCount L1.OP_SIZE = Args 1
opArgCount L1.OP_INVERT = UnusedOp
opArgCount L1.OP_AND = Args 2
opArgCount L1.OP_OR = Args 2
opArgCount L1.OP_XOR = Args 2
opArgCount L1.OP_EQUAL = Args 2
opArgCount L1.OP_EQUALVERIFY = Args 2
opArgCount L1.OP_RESERVED1 = UnusedOp
opArgCount L1.OP_RESERVED2 = UnusedOp
opArgCount L1.OP_1ADD = Args 1
opArgCount L1.OP_1SUB = Args 1
opArgCount L1.OP_2MUL = UnusedOp
opArgCount L1.OP_2DIV = UnusedOp
opArgCount L1.OP_NEGATE = Args 1
opArgCount L1.OP_ABS = Args 1
opArgCount L1.OP_NOT = Args 1
opArgCount L1.OP_0NOTEQUAL = Args 1
opArgCount L1.OP_ADD = Args 2
opArgCount L1.OP_SUB = Args 2
opArgCount L1.OP_MUL = Args 2
opArgCount L1.OP_DIV = Args 2
opArgCount L1.OP_MOD = Args 2
opArgCount L1.OP_LSHIFT = UnusedOp
opArgCount L1.OP_RSHIFT = UnusedOp
opArgCount L1.OP_BOOLAND = Args 2
opArgCount L1.OP_BOOLOR = Args 2
opArgCount L1.OP_NUMEQUAL = Args 2
opArgCount L1.OP_NUMEQUALVERIFY = Args 2
opArgCount L1.OP_NUMNOTEQUAL = Args 2
opArgCount L1.OP_LESSTHAN = Args 2
opArgCount L1.OP_GREATERTHAN = Args 2
opArgCount L1.OP_LESSTHANOREQUAL = Args 2
opArgCount L1.OP_GREATERTHANOREQUAL = Args 2
opArgCount L1.OP_MIN = Args 2
opArgCount L1.OP_MAX = Args 2
opArgCount L1.OP_WITHIN = Args 3
opArgCount L1.OP_RIPEMD160 = Args 1
opArgCount L1.OP_SHA1 = Args 1
opArgCount L1.OP_SHA256 = Args 1
opArgCount L1.OP_HASH160 = Args 1
opArgCount L1.OP_HASH256 = Args 1
opArgCount L1.OP_CODESEPARATOR = ZeroArg
opArgCount L1.OP_CHECKSIG = Args 2
opArgCount L1.OP_CHECKSIGVERIFY = Args 2
opArgCount L1.OP_CHECKMULTISIG = Special
opArgCount L1.OP_CHECKMULTISIGVERIFY = Special
opArgCount L1.OP_NOP1 = ZeroArg
opArgCount L1.OP_CHECKLOCKTIMEVERIFY = Args 1
opArgCount L1.OP_CHECKSEQUENCEVERIFY = Args 1
opArgCount L1.OP_NOP4 = ZeroArg
opArgCount L1.OP_NOP5 = ZeroArg
opArgCount L1.OP_NOP6 = ZeroArg
opArgCount L1.OP_NOP7 = ZeroArg
opArgCount L1.OP_NOP8 = ZeroArg
opArgCount L1.OP_NOP9 = ZeroArg
opArgCount L1.OP_NOP10 = ZeroArg
opArgCount L1.OP_CHECKDATASIG = Args 3
opArgCount L1.OP_CHECKDATASIGVERIFY = Args 3
opArgCount L1.OP_REVERSEBYTES = Args 1
opArgCount L1.OP_AVAILABLE_BD = UnusedOp
opArgCount L1.OP_AVAILABLE_BE = UnusedOp
opArgCount L1.OP_AVAILABLE_BF = UnusedOp
opArgCount L1.OP_INPUTINDEX = ZeroArg
opArgCount L1.OP_ACTIVEBYTECODE = ZeroArg
opArgCount L1.OP_TXVERSION = ZeroArg
opArgCount L1.OP_TXINPUTCOUNT = ZeroArg
opArgCount L1.OP_TXOUTPUTCOUNT = ZeroArg
opArgCount L1.OP_TXLOCKTIME = ZeroArg
opArgCount L1.OP_UTXOVALUE = Args 1
opArgCount L1.OP_UTXOBYTECODE = Args 1
opArgCount L1.OP_OUTPOINTTXHASH = Args 1
opArgCount L1.OP_OUTPOINTINDEX = Args 1
opArgCount L1.OP_INPUTBYTECODE = Args 1
opArgCount L1.OP_INPUTSEQUENCENUMBER = Args 1
opArgCount L1.OP_OUTPUTVALUE = Args 1
opArgCount L1.OP_OUTPUTBYTECODE = Args 1
opArgCount L1.OP_UTXOTOKENCATEGORY = Args 1
opArgCount L1.OP_UTXOTOKENCOMMITMENT = Args 1
opArgCount L1.OP_UTXOTOKENAMOUNT = Args 1
opArgCount L1.OP_OUTPUTTOKENCATEGORY = Args 1
opArgCount L1.OP_OUTPUTTOKENCOMMITMENT = Args 1
opArgCount L1.OP_OUTPUTTOKENAMOUNT = Args 1
opArgCount L1.OP_RESERVED3 = UnusedOp
opArgCount L1.OP_RESERVED4 = UnusedOp
opArgCount L1.OP_UNASSIGNED_D6 = UnusedOp
opArgCount L1.OP_UNASSIGNED_D7 = UnusedOp
opArgCount L1.OP_UNASSIGNED_D8 = UnusedOp
opArgCount L1.OP_UNASSIGNED_D9 = UnusedOp
opArgCount L1.OP_UNASSIGNED_DA = UnusedOp
opArgCount L1.OP_UNASSIGNED_DB = UnusedOp
opArgCount L1.OP_UNASSIGNED_DC = UnusedOp
opArgCount L1.OP_UNASSIGNED_DD = UnusedOp
opArgCount L1.OP_UNASSIGNED_DE = UnusedOp
opArgCount L1.OP_UNASSIGNED_DF = UnusedOp
opArgCount L1.OP_UNASSIGNED_E0 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E1 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E2 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E3 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E4 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E5 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E6 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E7 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E8 = UnusedOp
opArgCount L1.OP_UNASSIGNED_E9 = UnusedOp
opArgCount L1.OP_UNASSIGNED_EA = UnusedOp
opArgCount L1.OP_UNASSIGNED_EB = UnusedOp
opArgCount L1.OP_UNASSIGNED_EC = UnusedOp
opArgCount L1.OP_UNASSIGNED_ED = UnusedOp
opArgCount L1.OP_UNASSIGNED_EE = UnusedOp
opArgCount L1.OP_UNASSIGNED_EF = UnusedOp
opArgCount L1.OP_UNASSIGNED_F0 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F1 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F2 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F3 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F4 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F5 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F6 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F7 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F8 = UnusedOp
opArgCount L1.OP_UNASSIGNED_F9 = UnusedOp
opArgCount L1.OP_UNASSIGNED_FA = UnusedOp
opArgCount L1.OP_UNASSIGNED_FB = UnusedOp
opArgCount L1.OP_UNASSIGNED_FC = UnusedOp
opArgCount L1.OP_UNASSIGNED_FD = UnusedOp
opArgCount L1.OP_UNASSIGNED_FE = UnusedOp
opArgCount L1.OP_UNASSIGNED_FF = UnusedOp
