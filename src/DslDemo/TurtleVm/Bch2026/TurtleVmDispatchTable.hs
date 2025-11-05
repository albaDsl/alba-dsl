-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmDispatchTable (initOpDispatch) where

import Alba.Dsl.V1.Bch2025.OpsUntyped qualified as UT
import Alba.Dsl.V1.Bch2026
  ( FNC,
    begin,
    bytes,
    opDefineIdx,
    progBytes,
    (#),
  )
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import DslDemo.TurtleVm.Bch2026.TurtleOpAltStack (fromAltStack, toAltStack)
import DslDemo.TurtleVm.Bch2026.TurtleOpIf (ifOp)
import DslDemo.TurtleVm.Bch2026.TurtleVmState (dropCondStack, toggleCondStack)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (unsupportedOpBytes)

initOpDispatch :: Int -> FNC
initOpDispatch maxCsDepth =
  begin
    # (bytes [0x4f] # opDefineIdx 0x4f) -- OP_1NEGATE
    # (unsup # opDefineIdx 0x50) -- OP_RESERVED disabled op
    # (bytes [0x51] # opDefineIdx 0x51) -- OP_1
    # (bytes [0x52] # opDefineIdx 0x52) -- OP_2
    # (bytes [0x53] # opDefineIdx 0x53) -- OP_3
    # (bytes [0x54] # opDefineIdx 0x54) -- OP_4
    # (bytes [0x55] # opDefineIdx 0x55) -- OP_5
    # (bytes [0x56] # opDefineIdx 0x56) -- OP_6
    # (bytes [0x57] # opDefineIdx 0x57) -- OP_7
    # (bytes [0x58] # opDefineIdx 0x58) -- OP_8
    # (bytes [0x59] # opDefineIdx 0x59) -- OP_9
    # (bytes [0x5a] # opDefineIdx 0x5a) -- OP_10
    # (bytes [0x5b] # opDefineIdx 0x5b) -- OP_11
    # (bytes [0x5c] # opDefineIdx 0x5c) -- OP_12
    # (bytes [0x5d] # opDefineIdx 0x5d) -- OP_13
    # (bytes [0x5e] # opDefineIdx 0x5e) -- OP_14
    # (bytes [0x5f] # opDefineIdx 0x5f) -- OP_15
    # (bytes [0x60] # opDefineIdx 0x60) -- OP_16
    # (bytes [0x61] # opDefineIdx 0x61) -- OP_NOP
    # (unsup # opDefineIdx 0x62) -- OP_VER_OP_EVAL disabled op
    # ( begin
          # progBytes (toTyped (ifOp maxCsDepth id))
          # opDefineIdx 0x63 -- OP_IF
      )
    # ( begin
          # progBytes (toTyped (ifOp maxCsDepth UT.opNot))
          # opDefineIdx 0x64 -- OP_NOTIF
      )
    # (unsup # opDefineIdx 0x65) -- OP_VERIF_OP_BEGIN disabled op
    # (unsup # opDefineIdx 0x66) -- OP_VERNOTIF_OP_UNTIL disabled op
    # (progBytes toggleCondStack # opDefineIdx 0x67) -- OP_ELSE
    # (progBytes dropCondStack # opDefineIdx 0x68) -- OP_ENDIF
    # (bytes [0x69] # opDefineIdx 0x69) -- OP_VERIFY
    # (bytes [0x6a] # opDefineIdx 0x6a) -- OP_RETURN
    # (progBytes (toTyped toAltStack) # opDefineIdx 0x6b) -- OP_TOALTSTACK
    # (progBytes (toTyped fromAltStack) # opDefineIdx 0x6c) -- OP_FROMALTSTACK
    # (bytes [0x6d] # opDefineIdx 0x6d) -- OP_2DROP
    # (bytes [0x6e] # opDefineIdx 0x6e) -- OP_2DUP
    # (bytes [0x6f] # opDefineIdx 0x6f) -- OP_3DUP
    # (bytes [0x70] # opDefineIdx 0x70) -- OP_2OVER
    # (bytes [0x71] # opDefineIdx 0x71) -- OP_2ROT
    # (bytes [0x72] # opDefineIdx 0x72) -- OP_2SWAP
    # (bytes [0x73] # opDefineIdx 0x73) -- OP_IFDUP
    # (bytes [0x74] # opDefineIdx 0x74) -- OP_DEPTH
    # (bytes [0x75] # opDefineIdx 0x75) -- OP_DROP
    # (bytes [0x76] # opDefineIdx 0x76) -- OP_DUP
    # (bytes [0x77] # opDefineIdx 0x77) -- OP_NIP
    # (bytes [0x78] # opDefineIdx 0x78) -- OP_OVER
    # (bytes [0x79] # opDefineIdx 0x79) -- OP_PICK
    # (bytes [0x7a] # opDefineIdx 0x7a) -- OP_ROLL
    # (bytes [0x7b] # opDefineIdx 0x7b) -- OP_ROT
    # (bytes [0x7c] # opDefineIdx 0x7c) -- OP_SWAP
    # (bytes [0x7d] # opDefineIdx 0x7d) -- OP_TUCK
    # (bytes [0x7e] # opDefineIdx 0x7e) -- OP_CAT
    # (bytes [0x7f] # opDefineIdx 0x7f) -- OP_SPLIT
    # (bytes [0x80] # opDefineIdx 0x80) -- OP_NUM2BIN
    # (bytes [0x81] # opDefineIdx 0x81) -- OP_BIN2NUM
    # (bytes [0x82] # opDefineIdx 0x82) -- OP_SIZE
    # (unsup # opDefineIdx 0x83) -- OP_INVERT disabled op
    # (bytes [0x84] # opDefineIdx 0x84) -- OP_AND
    # (bytes [0x85] # opDefineIdx 0x85) -- OP_OR
    # (bytes [0x86] # opDefineIdx 0x86) -- OP_XOR
    # (bytes [0x87] # opDefineIdx 0x87) -- OP_EQUAL
    # (bytes [0x88] # opDefineIdx 0x88) -- OP_EQUALVERIFY
    # (unsup # opDefineIdx 0x89) -- OP_RESERVED1_OP_DEFINE disabled op
    # (unsup # opDefineIdx 0x8a) -- OP_RESERVED2_OP_INVOKE disabled op
    # (bytes [0x8b] # opDefineIdx 0x8b) -- OP_1ADD
    # (bytes [0x8c] # opDefineIdx 0x8c) -- OP_1SUB
    # (unsup # opDefineIdx 0x8d) -- OP_2MUL_OP_LSHIFTNUM disabled op
    # (unsup # opDefineIdx 0x8e) -- OP_2DIV_OP_RSHIFTNUM disabled op
    # (bytes [0x8f] # opDefineIdx 0x8f) -- OP_NEGATE
    # (bytes [0x90] # opDefineIdx 0x90) -- OP_ABS
    # (bytes [0x91] # opDefineIdx 0x91) -- OP_NOT
    # (bytes [0x92] # opDefineIdx 0x92) -- OP_0NOTEQUAL
    # (bytes [0x93] # opDefineIdx 0x93) -- OP_ADD
    # (bytes [0x94] # opDefineIdx 0x94) -- OP_SUB
    # (bytes [0x95] # opDefineIdx 0x95) -- OP_MUL
    # (bytes [0x96] # opDefineIdx 0x96) -- OP_DIV
    # (bytes [0x97] # opDefineIdx 0x97) -- OP_MOD
    # (unsup # opDefineIdx 0x98) -- OP_LSHIFT_OP_LSHIFTBIN disabled op
    # (unsup # opDefineIdx 0x99) -- OP_RSHIFT_OP_RSHIFTBIN disabled op
    # (bytes [0x9a] # opDefineIdx 0x9a) -- OP_BOOLAND
    # (bytes [0x9b] # opDefineIdx 0x9b) -- OP_BOOLOR
    # (bytes [0x9c] # opDefineIdx 0x9c) -- OP_NUMEQUAL
    # (bytes [0x9d] # opDefineIdx 0x9d) -- OP_NUMEQUALVERIFY
    # (bytes [0x9e] # opDefineIdx 0x9e) -- OP_NUMNOTEQUAL
    # (bytes [0x9f] # opDefineIdx 0x9f) -- OP_LESSTHAN
    # (bytes [0xa0] # opDefineIdx 0xa0) -- OP_GREATERTHAN
    # (bytes [0xa1] # opDefineIdx 0xa1) -- OP_LESSTHANOREQUAL
    # (bytes [0xa2] # opDefineIdx 0xa2) -- OP_GREATERTHANOREQUAL
    # (bytes [0xa3] # opDefineIdx 0xa3) -- OP_MIN
    # (bytes [0xa4] # opDefineIdx 0xa4) -- OP_MAX
    # (bytes [0xa5] # opDefineIdx 0xa5) -- OP_WITHIN
    # (bytes [0xa6] # opDefineIdx 0xa6) -- OP_RIPEMD160
    # (bytes [0xa7] # opDefineIdx 0xa7) -- OP_SHA1
    # (bytes [0xa8] # opDefineIdx 0xa8) -- OP_SHA256
    # (bytes [0xa9] # opDefineIdx 0xa9) -- OP_HASH160
    # (bytes [0xaa] # opDefineIdx 0xaa) -- OP_HASH256
    # (unsup # opDefineIdx 0xab) -- OP_CODESEPARATOR not implemented
    # (bytes [0xac] # opDefineIdx 0xac) -- OP_CHECKSIG
    # (bytes [0xad] # opDefineIdx 0xad) -- OP_CHECKSIGVERIFY
    # (bytes [0xae] # opDefineIdx 0xae) -- OP_CHECKMULTISIG
    # (bytes [0xaf] # opDefineIdx 0xaf) -- OP_CHECKMULTISIGVERIFY
    # (unsup # opDefineIdx 0xb0) -- OP_NOP1
    # (bytes [0xb1] # opDefineIdx 0xb1) -- OP_CHECKLOCKTIMEVERIFY
    # (bytes [0xb2] # opDefineIdx 0xb2) -- OP_CHECKSEQUENCEVERIFY
    # (unsup # opDefineIdx 0xb3) -- OP_NOP4
    # (unsup # opDefineIdx 0xb4) -- OP_NOP5
    # (unsup # opDefineIdx 0xb5) -- OP_NOP6
    # (unsup # opDefineIdx 0xb6) -- OP_NOP7
    # (unsup # opDefineIdx 0xb7) -- OP_NOP8
    # (unsup # opDefineIdx 0xb8) -- OP_NOP9
    # (unsup # opDefineIdx 0xb9) -- OP_NOP10
    # (bytes [0xba] # opDefineIdx 0xba) -- OP_CHECKDATASIG
    # (bytes [0xbb] # opDefineIdx 0xbb) -- OP_CHECKDATASIGVERIFY
    # (bytes [0xbc] # opDefineIdx 0xbc) -- OP_REVERSEBYTES
    # (unsup # opDefineIdx 0xbd) -- OP_AVAILABLE_BD
    # (unsup # opDefineIdx 0xbe) -- OP_AVAILABLE_BE
    # (unsup # opDefineIdx 0xbf) -- OP_AVAILABLE_BF
    # (bytes [0xc0] # opDefineIdx 0xc0) -- OP_INPUTINDEX
    # (unsup # opDefineIdx 0xc1) -- OP_ACTIVEBYTECODE not implemented
    # (bytes [0xc2] # opDefineIdx 0xc2) -- OP_TXVERSION
    # (bytes [0xc3] # opDefineIdx 0xc3) -- OP_TXINPUTCOUNT
    # (bytes [0xc4] # opDefineIdx 0xc4) -- OP_TXOUTPUTCOUNT
    # (bytes [0xc5] # opDefineIdx 0xc5) -- OP_TXLOCKTIME
    # (bytes [0xc6] # opDefineIdx 0xc6) -- OP_UTXOVALUE
    # (bytes [0xc7] # opDefineIdx 0xc7) -- OP_UTXOBYTECODE
    # (bytes [0xc8] # opDefineIdx 0xc8) -- OP_OUTPOINTTXHASH
    # (bytes [0xc9] # opDefineIdx 0xc9) -- OP_OUTPOINTINDEX
    # (bytes [0xca] # opDefineIdx 0xca) -- OP_INPUTBYTECODE
    # (bytes [0xcb] # opDefineIdx 0xcb) -- OP_INPUTSEQUENCENUMBER
    # (bytes [0xcc] # opDefineIdx 0xcc) -- OP_OUTPUTVALUE
    # (bytes [0xcd] # opDefineIdx 0xcd) -- OP_OUTPUTBYTECODE
    # (bytes [0xce] # opDefineIdx 0xce) -- OP_UTXOTOKENCATEGORY
    # (bytes [0xcf] # opDefineIdx 0xcf) -- OP_UTXOTOKENCOMMITMENT
    # (bytes [0xd0] # opDefineIdx 0xd0) -- OP_UTXOTOKENAMOUNT
    # (bytes [0xd1] # opDefineIdx 0xd1) -- OP_OUTPUTTOKENCATEGORY
    # (bytes [0xd2] # opDefineIdx 0xd2) -- OP_OUTPUTTOKENCOMMITMENT
    # (bytes [0xd3] # opDefineIdx 0xd3) -- OP_OUTPUTTOKENAMOUNT
  where
    unsup = unsupportedOpBytes
