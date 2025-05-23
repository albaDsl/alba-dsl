-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestOpcodes (testOpcodes) where

import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..), opcodeL1ToWord8)
import Alba.Vm.Common.OpcodeL2 (codeL2ToCodeL1, getOp)
import Data.ByteString qualified as B
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testOpcodes :: TestTree
testOpcodes =
  testGroup
    "Opcode enums"
    [ testCase "Encode / decode" $
        fromIntegral . convert . fromIntegral <$> byteVals @?= byteVals,
      testCase "Absolute values" $
        (toEnum . fromIntegral <$> byteVals) @?= (expectedOpcode <$> byteVals),
      testCase "L1/L2 conversion" $
        mapM_
          ( \l1Op ->
              let codeL1 = [opcodeL1ToWord8 l1Op] :: B.ByteString
                  Just (l2Op, _) = getOp codeL1
               in Just codeL1 @?= codeL2ToCodeL1 [l2Op]
          )
          ([OP_1NEGATE .. OP_UNASSIGNED_FF] :: [OpcodeL1])
    ]
  where
    byteVals = [0 .. 255] :: [Word8]

    convert :: Int -> Int
    convert x =
      let x' = toEnum x :: OpcodeL1
       in fromEnum x'

expectedOpcode :: Word8 -> OpcodeL1
expectedOpcode 0x0 = OP_0
expectedOpcode 0x1 = OP_DATA_01
expectedOpcode 0x2 = OP_DATA_02
expectedOpcode 0x3 = OP_DATA_03
expectedOpcode 0x4 = OP_DATA_04
expectedOpcode 0x5 = OP_DATA_05
expectedOpcode 0x6 = OP_DATA_06
expectedOpcode 0x7 = OP_DATA_07
expectedOpcode 0x8 = OP_DATA_08
expectedOpcode 0x9 = OP_DATA_09
expectedOpcode 0xA = OP_DATA_10
expectedOpcode 0xB = OP_DATA_11
expectedOpcode 0xC = OP_DATA_12
expectedOpcode 0xD = OP_DATA_13
expectedOpcode 0xE = OP_DATA_14
expectedOpcode 0xF = OP_DATA_15
expectedOpcode 0x10 = OP_DATA_16
expectedOpcode 0x11 = OP_DATA_17
expectedOpcode 0x12 = OP_DATA_18
expectedOpcode 0x13 = OP_DATA_19
expectedOpcode 0x14 = OP_DATA_20
expectedOpcode 0x15 = OP_DATA_21
expectedOpcode 0x16 = OP_DATA_22
expectedOpcode 0x17 = OP_DATA_23
expectedOpcode 0x18 = OP_DATA_24
expectedOpcode 0x19 = OP_DATA_25
expectedOpcode 0x1A = OP_DATA_26
expectedOpcode 0x1B = OP_DATA_27
expectedOpcode 0x1C = OP_DATA_28
expectedOpcode 0x1D = OP_DATA_29
expectedOpcode 0x1E = OP_DATA_30
expectedOpcode 0x1F = OP_DATA_31
expectedOpcode 0x20 = OP_DATA_32
expectedOpcode 0x21 = OP_DATA_33
expectedOpcode 0x22 = OP_DATA_34
expectedOpcode 0x23 = OP_DATA_35
expectedOpcode 0x24 = OP_DATA_36
expectedOpcode 0x25 = OP_DATA_37
expectedOpcode 0x26 = OP_DATA_38
expectedOpcode 0x27 = OP_DATA_39
expectedOpcode 0x28 = OP_DATA_40
expectedOpcode 0x29 = OP_DATA_41
expectedOpcode 0x2A = OP_DATA_42
expectedOpcode 0x2B = OP_DATA_43
expectedOpcode 0x2C = OP_DATA_44
expectedOpcode 0x2D = OP_DATA_45
expectedOpcode 0x2E = OP_DATA_46
expectedOpcode 0x2F = OP_DATA_47
expectedOpcode 0x30 = OP_DATA_48
expectedOpcode 0x31 = OP_DATA_49
expectedOpcode 0x32 = OP_DATA_50
expectedOpcode 0x33 = OP_DATA_51
expectedOpcode 0x34 = OP_DATA_52
expectedOpcode 0x35 = OP_DATA_53
expectedOpcode 0x36 = OP_DATA_54
expectedOpcode 0x37 = OP_DATA_55
expectedOpcode 0x38 = OP_DATA_56
expectedOpcode 0x39 = OP_DATA_57
expectedOpcode 0x3A = OP_DATA_58
expectedOpcode 0x3B = OP_DATA_59
expectedOpcode 0x3C = OP_DATA_60
expectedOpcode 0x3D = OP_DATA_61
expectedOpcode 0x3E = OP_DATA_62
expectedOpcode 0x3F = OP_DATA_63
expectedOpcode 0x40 = OP_DATA_64
expectedOpcode 0x41 = OP_DATA_65
expectedOpcode 0x42 = OP_DATA_66
expectedOpcode 0x43 = OP_DATA_67
expectedOpcode 0x44 = OP_DATA_68
expectedOpcode 0x45 = OP_DATA_69
expectedOpcode 0x46 = OP_DATA_70
expectedOpcode 0x47 = OP_DATA_71
expectedOpcode 0x48 = OP_DATA_72
expectedOpcode 0x49 = OP_DATA_73
expectedOpcode 0x4A = OP_DATA_74
expectedOpcode 0x4B = OP_DATA_75
expectedOpcode 0x4C = OP_PUSHDATA1
expectedOpcode 0x4D = OP_PUSHDATA2
expectedOpcode 0x4E = OP_PUSHDATA4
expectedOpcode 0x4F = OP_1NEGATE
expectedOpcode 0x50 = OP_RESERVED
expectedOpcode 0x51 = OP_1
expectedOpcode 0x52 = OP_2
expectedOpcode 0x53 = OP_3
expectedOpcode 0x54 = OP_4
expectedOpcode 0x55 = OP_5
expectedOpcode 0x56 = OP_6
expectedOpcode 0x57 = OP_7
expectedOpcode 0x58 = OP_8
expectedOpcode 0x59 = OP_9
expectedOpcode 0x5A = OP_10
expectedOpcode 0x5B = OP_11
expectedOpcode 0x5C = OP_12
expectedOpcode 0x5D = OP_13
expectedOpcode 0x5E = OP_14
expectedOpcode 0x5F = OP_15
expectedOpcode 0x60 = OP_16
expectedOpcode 0x61 = OP_NOP
expectedOpcode 0x62 = OP_VER_OP_EVAL
expectedOpcode 0x63 = OP_IF
expectedOpcode 0x64 = OP_NOTIF
expectedOpcode 0x65 = OP_VERIF
expectedOpcode 0x66 = OP_VERNOTIF
expectedOpcode 0x67 = OP_ELSE
expectedOpcode 0x68 = OP_ENDIF
expectedOpcode 0x69 = OP_VERIFY
expectedOpcode 0x6A = OP_RETURN
expectedOpcode 0x6B = OP_TOALTSTACK
expectedOpcode 0x6C = OP_FROMALTSTACK
expectedOpcode 0x6D = OP_2DROP
expectedOpcode 0x6E = OP_2DUP
expectedOpcode 0x6F = OP_3DUP
expectedOpcode 0x70 = OP_2OVER
expectedOpcode 0x71 = OP_2ROT
expectedOpcode 0x72 = OP_2SWAP
expectedOpcode 0x73 = OP_IFDUP
expectedOpcode 0x74 = OP_DEPTH
expectedOpcode 0x75 = OP_DROP
expectedOpcode 0x76 = OP_DUP
expectedOpcode 0x77 = OP_NIP
expectedOpcode 0x78 = OP_OVER
expectedOpcode 0x79 = OP_PICK
expectedOpcode 0x7A = OP_ROLL
expectedOpcode 0x7B = OP_ROT
expectedOpcode 0x7C = OP_SWAP
expectedOpcode 0x7D = OP_TUCK
expectedOpcode 0x7E = OP_CAT
expectedOpcode 0x7F = OP_SPLIT
expectedOpcode 0x80 = OP_NUM2BIN
expectedOpcode 0x81 = OP_BIN2NUM
expectedOpcode 0x82 = OP_SIZE
expectedOpcode 0x83 = OP_INVERT
expectedOpcode 0x84 = OP_AND
expectedOpcode 0x85 = OP_OR
expectedOpcode 0x86 = OP_XOR
expectedOpcode 0x87 = OP_EQUAL
expectedOpcode 0x88 = OP_EQUALVERIFY
expectedOpcode 0x89 = OP_RESERVED1
expectedOpcode 0x8A = OP_RESERVED2
expectedOpcode 0x8B = OP_1ADD
expectedOpcode 0x8C = OP_1SUB
expectedOpcode 0x8D = OP_2MUL
expectedOpcode 0x8E = OP_2DIV
expectedOpcode 0x8F = OP_NEGATE
expectedOpcode 0x90 = OP_ABS
expectedOpcode 0x91 = OP_NOT
expectedOpcode 0x92 = OP_0NOTEQUAL
expectedOpcode 0x93 = OP_ADD
expectedOpcode 0x94 = OP_SUB
expectedOpcode 0x95 = OP_MUL
expectedOpcode 0x96 = OP_DIV
expectedOpcode 0x97 = OP_MOD
expectedOpcode 0x98 = OP_LSHIFT
expectedOpcode 0x99 = OP_RSHIFT
expectedOpcode 0x9A = OP_BOOLAND
expectedOpcode 0x9B = OP_BOOLOR
expectedOpcode 0x9C = OP_NUMEQUAL
expectedOpcode 0x9D = OP_NUMEQUALVERIFY
expectedOpcode 0x9E = OP_NUMNOTEQUAL
expectedOpcode 0x9F = OP_LESSTHAN
expectedOpcode 0xA0 = OP_GREATERTHAN
expectedOpcode 0xA1 = OP_LESSTHANOREQUAL
expectedOpcode 0xA2 = OP_GREATERTHANOREQUAL
expectedOpcode 0xA3 = OP_MIN
expectedOpcode 0xA4 = OP_MAX
expectedOpcode 0xA5 = OP_WITHIN
expectedOpcode 0xA6 = OP_RIPEMD160
expectedOpcode 0xA7 = OP_SHA1
expectedOpcode 0xA8 = OP_SHA256
expectedOpcode 0xA9 = OP_HASH160
expectedOpcode 0xAA = OP_HASH256
expectedOpcode 0xAB = OP_CODESEPARATOR
expectedOpcode 0xAC = OP_CHECKSIG
expectedOpcode 0xAD = OP_CHECKSIGVERIFY
expectedOpcode 0xAE = OP_CHECKMULTISIG
expectedOpcode 0xAF = OP_CHECKMULTISIGVERIFY
expectedOpcode 0xB0 = OP_NOP1
expectedOpcode 0xB1 = OP_CHECKLOCKTIMEVERIFY
expectedOpcode 0xB2 = OP_CHECKSEQUENCEVERIFY
expectedOpcode 0xB3 = OP_NOP4
expectedOpcode 0xB4 = OP_NOP5
expectedOpcode 0xB5 = OP_NOP6
expectedOpcode 0xB6 = OP_NOP7
expectedOpcode 0xB7 = OP_NOP8
expectedOpcode 0xB8 = OP_NOP9
expectedOpcode 0xB9 = OP_NOP10
expectedOpcode 0xBA = OP_CHECKDATASIG
expectedOpcode 0xBB = OP_CHECKDATASIGVERIFY
expectedOpcode 0xBC = OP_REVERSEBYTES
expectedOpcode 0xBD = OP_AVAILABLE_BD
expectedOpcode 0xBE = OP_AVAILABLE_BE
expectedOpcode 0xBF = OP_AVAILABLE_BF
expectedOpcode 0xC0 = OP_INPUTINDEX
expectedOpcode 0xC1 = OP_ACTIVEBYTECODE
expectedOpcode 0xC2 = OP_TXVERSION
expectedOpcode 0xC3 = OP_TXINPUTCOUNT
expectedOpcode 0xC4 = OP_TXOUTPUTCOUNT
expectedOpcode 0xC5 = OP_TXLOCKTIME
expectedOpcode 0xC6 = OP_UTXOVALUE
expectedOpcode 0xC7 = OP_UTXOBYTECODE
expectedOpcode 0xC8 = OP_OUTPOINTTXHASH
expectedOpcode 0xC9 = OP_OUTPOINTINDEX
expectedOpcode 0xCA = OP_INPUTBYTECODE
expectedOpcode 0xCB = OP_INPUTSEQUENCENUMBER
expectedOpcode 0xCC = OP_OUTPUTVALUE
expectedOpcode 0xCD = OP_OUTPUTBYTECODE
expectedOpcode 0xCE = OP_UTXOTOKENCATEGORY
expectedOpcode 0xCF = OP_UTXOTOKENCOMMITMENT
expectedOpcode 0xD0 = OP_UTXOTOKENAMOUNT
expectedOpcode 0xD1 = OP_OUTPUTTOKENCATEGORY
expectedOpcode 0xD2 = OP_OUTPUTTOKENCOMMITMENT
expectedOpcode 0xD3 = OP_OUTPUTTOKENAMOUNT
expectedOpcode 0xD4 = OP_RESERVED3
expectedOpcode 0xD5 = OP_RESERVED4
expectedOpcode 0xD6 = OP_UNASSIGNED_D6
expectedOpcode 0xD7 = OP_UNASSIGNED_D7
expectedOpcode 0xD8 = OP_UNASSIGNED_D8
expectedOpcode 0xD9 = OP_UNASSIGNED_D9
expectedOpcode 0xDA = OP_UNASSIGNED_DA
expectedOpcode 0xDB = OP_UNASSIGNED_DB
expectedOpcode 0xDC = OP_UNASSIGNED_DC
expectedOpcode 0xDD = OP_UNASSIGNED_DD
expectedOpcode 0xDE = OP_UNASSIGNED_DE
expectedOpcode 0xDF = OP_UNASSIGNED_DF
expectedOpcode 0xE0 = OP_UNASSIGNED_E0
expectedOpcode 0xE1 = OP_UNASSIGNED_E1
expectedOpcode 0xE2 = OP_UNASSIGNED_E2
expectedOpcode 0xE3 = OP_UNASSIGNED_E3
expectedOpcode 0xE4 = OP_UNASSIGNED_E4
expectedOpcode 0xE5 = OP_UNASSIGNED_E5
expectedOpcode 0xE6 = OP_UNASSIGNED_E6
expectedOpcode 0xE7 = OP_UNASSIGNED_E7
expectedOpcode 0xE8 = OP_UNASSIGNED_E8
expectedOpcode 0xE9 = OP_UNASSIGNED_E9
expectedOpcode 0xEA = OP_UNASSIGNED_EA
expectedOpcode 0xEB = OP_UNASSIGNED_EB
expectedOpcode 0xEC = OP_UNASSIGNED_EC
expectedOpcode 0xED = OP_UNASSIGNED_ED
expectedOpcode 0xEE = OP_UNASSIGNED_EE
expectedOpcode 0xEF = OP_UNASSIGNED_EF
expectedOpcode 0xF0 = OP_UNASSIGNED_F0
expectedOpcode 0xF1 = OP_UNASSIGNED_F1
expectedOpcode 0xF2 = OP_UNASSIGNED_F2
expectedOpcode 0xF3 = OP_UNASSIGNED_F3
expectedOpcode 0xF4 = OP_UNASSIGNED_F4
expectedOpcode 0xF5 = OP_UNASSIGNED_F5
expectedOpcode 0xF6 = OP_UNASSIGNED_F6
expectedOpcode 0xF7 = OP_UNASSIGNED_F7
expectedOpcode 0xF8 = OP_UNASSIGNED_F8
expectedOpcode 0xF9 = OP_UNASSIGNED_F9
expectedOpcode 0xFA = OP_UNASSIGNED_FA
expectedOpcode 0xFB = OP_UNASSIGNED_FB
expectedOpcode 0xFC = OP_UNASSIGNED_FC
expectedOpcode 0xFD = OP_UNASSIGNED_FD
expectedOpcode 0xFE = OP_UNASSIGNED_FE
expectedOpcode 0xFF = OP_UNASSIGNED_FF
