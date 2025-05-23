-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.OpcodeL2
  ( OpcodeL2 (..),
    CodeL2,
    codeL2ToCodeL1,
    bytesToDataOp,
    getOp,
    isMinimal,
  )
where

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1 (CodeL1, OpcodeL1)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Binary.Get (getWord16le, getWord32le, runGet)
import Data.Binary.Put (putWord16le, putWord32le, runPut)
import Data.ByteString qualified as B
import Data.Foldable (foldrM)
import Data.Sequence qualified as S
import Data.Word (Word16, Word32, Word8)
import Prelude hiding (splitAt)

-- Higher-level bytecode format where all data push ops are grouped under
-- OP_DATA, and all disabled and do-not-use ops are grouped under OP_UNUSED.
data OpcodeL2
  = OP_0
  | OP_DATA OpcodeL1 Bytes
  | OP_1NEGATE
  | OP_1
  | OP_2
  | OP_3
  | OP_4
  | OP_5
  | OP_6
  | OP_7
  | OP_8
  | OP_9
  | OP_10
  | OP_11
  | OP_12
  | OP_13
  | OP_14
  | OP_15
  | OP_16
  | OP_NOP
  | OP_EVAL
  | OP_IF
  | OP_NOTIF
  | OP_ELSE
  | OP_ENDIF
  | OP_VERIFY
  | OP_RETURN
  | OP_TOALTSTACK
  | OP_FROMALTSTACK
  | OP_2DROP
  | OP_2DUP
  | OP_3DUP
  | OP_2OVER
  | OP_2ROT
  | OP_2SWAP
  | OP_IFDUP
  | OP_DEPTH
  | OP_DROP
  | OP_DUP
  | OP_NIP
  | OP_OVER
  | OP_PICK
  | OP_ROLL
  | OP_ROT
  | OP_SWAP
  | OP_TUCK
  | OP_CAT
  | OP_SPLIT
  | OP_NUM2BIN
  | OP_BIN2NUM
  | OP_SIZE
  | OP_AND
  | OP_OR
  | OP_XOR
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_1ADD
  | OP_1SUB
  | OP_NEGATE
  | OP_ABS
  | OP_NOT
  | OP_0NOTEQUAL
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_MOD
  | OP_BOOLAND
  | OP_BOOLOR
  | OP_NUMEQUAL
  | OP_NUMEQUALVERIFY
  | OP_NUMNOTEQUAL
  | OP_LESSTHAN
  | OP_GREATERTHAN
  | OP_LESSTHANOREQUAL
  | OP_GREATERTHANOREQUAL
  | OP_MIN
  | OP_MAX
  | OP_WITHIN
  | OP_RIPEMD160
  | OP_SHA1
  | OP_SHA256
  | OP_HASH160
  | OP_HASH256
  | OP_CODESEPARATOR
  | OP_CHECKSIG
  | OP_CHECKSIGVERIFY
  | OP_CHECKMULTISIG
  | OP_CHECKMULTISIGVERIFY
  | OP_CHECKLOCKTIMEVERIFY
  | OP_CHECKSEQUENCEVERIFY
  | OP_CHECKDATASIG
  | OP_CHECKDATASIGVERIFY
  | OP_REVERSEBYTES
  | OP_INPUTINDEX
  | OP_ACTIVEBYTECODE
  | OP_TXVERSION
  | OP_TXINPUTCOUNT
  | OP_TXOUTPUTCOUNT
  | OP_TXLOCKTIME
  | OP_UTXOVALUE
  | OP_UTXOBYTECODE
  | OP_OUTPOINTTXHASH
  | OP_OUTPOINTINDEX
  | OP_INPUTBYTECODE
  | OP_INPUTSEQUENCENUMBER
  | OP_OUTPUTVALUE
  | OP_OUTPUTBYTECODE
  | OP_UTXOTOKENCATEGORY
  | OP_UTXOTOKENCOMMITMENT
  | OP_UTXOTOKENAMOUNT
  | OP_OUTPUTTOKENCATEGORY
  | OP_OUTPUTTOKENCOMMITMENT
  | OP_OUTPUTTOKENAMOUNT
  | OP_UNUSED OpcodeL1
  deriving (Eq, Show)

type CodeL2 = S.Seq OpcodeL2

codeL2ToCodeL1 :: CodeL2 -> Maybe CodeL1
codeL2ToCodeL1 =
  foldrM
    ( \x a -> do
        x' <- opcodeL2ToCodeL1 x
        pure $ x' <> a
    )
    B.empty

opcodeL2ToCodeL1 :: OpcodeL2 -> Maybe CodeL1
opcodeL2ToCodeL1 OP_0 = Just $ toCodeL1 L1.OP_0
opcodeL2ToCodeL1 (OP_DATA L1.OP_PUSHDATA1 bytes) = do
  let len = B.length bytes
  guard (len <= fromIntegral (maxBound :: Word8))
  Just $ toCodeL1 L1.OP_PUSHDATA1 <> B.singleton (fromIntegral len) <> bytes
opcodeL2ToCodeL1 (OP_DATA L1.OP_PUSHDATA2 bytes) = do
  let len = B.length bytes
  guard (len <= fromIntegral (maxBound :: Word16))
  Just $
    toCodeL1 L1.OP_PUSHDATA2
      <> B.toStrict (runPut (putWord16le (fromIntegral len)))
      <> bytes
opcodeL2ToCodeL1 (OP_DATA L1.OP_PUSHDATA4 bytes) = do
  let len = B.length bytes
  guard (len <= fromIntegral (maxBound :: Word32))
  Just $
    toCodeL1 L1.OP_PUSHDATA4
      <> B.toStrict (runPut (putWord32le (fromIntegral len)))
      <> bytes
opcodeL2ToCodeL1 (OP_DATA opcodeL1 bytes) =
  let len = B.length bytes
   in if opcodeL1 >= L1.OP_DATA_01
        && opcodeL1 <= L1.OP_DATA_75
        && fromEnum opcodeL1 == len
        then Just $ toCodeL1 opcodeL1 <> bytes
        else Nothing
opcodeL2ToCodeL1 OP_1NEGATE = Just $ toCodeL1 L1.OP_1NEGATE
opcodeL2ToCodeL1 OP_1 = Just $ toCodeL1 L1.OP_1
opcodeL2ToCodeL1 OP_2 = Just $ toCodeL1 L1.OP_2
opcodeL2ToCodeL1 OP_3 = Just $ toCodeL1 L1.OP_3
opcodeL2ToCodeL1 OP_4 = Just $ toCodeL1 L1.OP_4
opcodeL2ToCodeL1 OP_5 = Just $ toCodeL1 L1.OP_5
opcodeL2ToCodeL1 OP_6 = Just $ toCodeL1 L1.OP_6
opcodeL2ToCodeL1 OP_7 = Just $ toCodeL1 L1.OP_7
opcodeL2ToCodeL1 OP_8 = Just $ toCodeL1 L1.OP_8
opcodeL2ToCodeL1 OP_9 = Just $ toCodeL1 L1.OP_9
opcodeL2ToCodeL1 OP_10 = Just $ toCodeL1 L1.OP_10
opcodeL2ToCodeL1 OP_11 = Just $ toCodeL1 L1.OP_11
opcodeL2ToCodeL1 OP_12 = Just $ toCodeL1 L1.OP_12
opcodeL2ToCodeL1 OP_13 = Just $ toCodeL1 L1.OP_13
opcodeL2ToCodeL1 OP_14 = Just $ toCodeL1 L1.OP_14
opcodeL2ToCodeL1 OP_15 = Just $ toCodeL1 L1.OP_15
opcodeL2ToCodeL1 OP_16 = Just $ toCodeL1 L1.OP_16
opcodeL2ToCodeL1 OP_NOP = Just $ toCodeL1 L1.OP_NOP
opcodeL2ToCodeL1 OP_EVAL = Just $ toCodeL1 L1.OP_VER_OP_EVAL
opcodeL2ToCodeL1 OP_IF = Just $ toCodeL1 L1.OP_IF
opcodeL2ToCodeL1 OP_NOTIF = Just $ toCodeL1 L1.OP_NOTIF
opcodeL2ToCodeL1 OP_ELSE = Just $ toCodeL1 L1.OP_ELSE
opcodeL2ToCodeL1 OP_ENDIF = Just $ toCodeL1 L1.OP_ENDIF
opcodeL2ToCodeL1 OP_VERIFY = Just $ toCodeL1 L1.OP_VERIFY
opcodeL2ToCodeL1 OP_RETURN = Just $ toCodeL1 L1.OP_RETURN
opcodeL2ToCodeL1 OP_TOALTSTACK = Just $ toCodeL1 L1.OP_TOALTSTACK
opcodeL2ToCodeL1 OP_FROMALTSTACK = Just $ toCodeL1 L1.OP_FROMALTSTACK
opcodeL2ToCodeL1 OP_2DROP = Just $ toCodeL1 L1.OP_2DROP
opcodeL2ToCodeL1 OP_2DUP = Just $ toCodeL1 L1.OP_2DUP
opcodeL2ToCodeL1 OP_3DUP = Just $ toCodeL1 L1.OP_3DUP
opcodeL2ToCodeL1 OP_2OVER = Just $ toCodeL1 L1.OP_2OVER
opcodeL2ToCodeL1 OP_2ROT = Just $ toCodeL1 L1.OP_2ROT
opcodeL2ToCodeL1 OP_2SWAP = Just $ toCodeL1 L1.OP_2SWAP
opcodeL2ToCodeL1 OP_IFDUP = Just $ toCodeL1 L1.OP_IFDUP
opcodeL2ToCodeL1 OP_DEPTH = Just $ toCodeL1 L1.OP_DEPTH
opcodeL2ToCodeL1 OP_DROP = Just $ toCodeL1 L1.OP_DROP
opcodeL2ToCodeL1 OP_DUP = Just $ toCodeL1 L1.OP_DUP
opcodeL2ToCodeL1 OP_NIP = Just $ toCodeL1 L1.OP_NIP
opcodeL2ToCodeL1 OP_OVER = Just $ toCodeL1 L1.OP_OVER
opcodeL2ToCodeL1 OP_PICK = Just $ toCodeL1 L1.OP_PICK
opcodeL2ToCodeL1 OP_ROLL = Just $ toCodeL1 L1.OP_ROLL
opcodeL2ToCodeL1 OP_ROT = Just $ toCodeL1 L1.OP_ROT
opcodeL2ToCodeL1 OP_SWAP = Just $ toCodeL1 L1.OP_SWAP
opcodeL2ToCodeL1 OP_TUCK = Just $ toCodeL1 L1.OP_TUCK
opcodeL2ToCodeL1 OP_CAT = Just $ toCodeL1 L1.OP_CAT
opcodeL2ToCodeL1 OP_SPLIT = Just $ toCodeL1 L1.OP_SPLIT
opcodeL2ToCodeL1 OP_NUM2BIN = Just $ toCodeL1 L1.OP_NUM2BIN
opcodeL2ToCodeL1 OP_BIN2NUM = Just $ toCodeL1 L1.OP_BIN2NUM
opcodeL2ToCodeL1 OP_SIZE = Just $ toCodeL1 L1.OP_SIZE
opcodeL2ToCodeL1 OP_AND = Just $ toCodeL1 L1.OP_AND
opcodeL2ToCodeL1 OP_OR = Just $ toCodeL1 L1.OP_OR
opcodeL2ToCodeL1 OP_XOR = Just $ toCodeL1 L1.OP_XOR
opcodeL2ToCodeL1 OP_EQUAL = Just $ toCodeL1 L1.OP_EQUAL
opcodeL2ToCodeL1 OP_EQUALVERIFY = Just $ toCodeL1 L1.OP_EQUALVERIFY
opcodeL2ToCodeL1 OP_1ADD = Just $ toCodeL1 L1.OP_1ADD
opcodeL2ToCodeL1 OP_1SUB = Just $ toCodeL1 L1.OP_1SUB
opcodeL2ToCodeL1 OP_NEGATE = Just $ toCodeL1 L1.OP_NEGATE
opcodeL2ToCodeL1 OP_ABS = Just $ toCodeL1 L1.OP_ABS
opcodeL2ToCodeL1 OP_NOT = Just $ toCodeL1 L1.OP_NOT
opcodeL2ToCodeL1 OP_0NOTEQUAL = Just $ toCodeL1 L1.OP_0NOTEQUAL
opcodeL2ToCodeL1 OP_ADD = Just $ toCodeL1 L1.OP_ADD
opcodeL2ToCodeL1 OP_SUB = Just $ toCodeL1 L1.OP_SUB
opcodeL2ToCodeL1 OP_MUL = Just $ toCodeL1 L1.OP_MUL
opcodeL2ToCodeL1 OP_DIV = Just $ toCodeL1 L1.OP_DIV
opcodeL2ToCodeL1 OP_MOD = Just $ toCodeL1 L1.OP_MOD
opcodeL2ToCodeL1 OP_BOOLAND = Just $ toCodeL1 L1.OP_BOOLAND
opcodeL2ToCodeL1 OP_BOOLOR = Just $ toCodeL1 L1.OP_BOOLOR
opcodeL2ToCodeL1 OP_NUMEQUAL = Just $ toCodeL1 L1.OP_NUMEQUAL
opcodeL2ToCodeL1 OP_NUMEQUALVERIFY = Just $ toCodeL1 L1.OP_NUMEQUALVERIFY
opcodeL2ToCodeL1 OP_NUMNOTEQUAL = Just $ toCodeL1 L1.OP_NUMNOTEQUAL
opcodeL2ToCodeL1 OP_LESSTHAN = Just $ toCodeL1 L1.OP_LESSTHAN
opcodeL2ToCodeL1 OP_GREATERTHAN = Just $ toCodeL1 L1.OP_GREATERTHAN
opcodeL2ToCodeL1 OP_LESSTHANOREQUAL = Just $ toCodeL1 L1.OP_LESSTHANOREQUAL
opcodeL2ToCodeL1 OP_GREATERTHANOREQUAL =
  Just $ toCodeL1 L1.OP_GREATERTHANOREQUAL
opcodeL2ToCodeL1 OP_MIN = Just $ toCodeL1 L1.OP_MIN
opcodeL2ToCodeL1 OP_MAX = Just $ toCodeL1 L1.OP_MAX
opcodeL2ToCodeL1 OP_WITHIN = Just $ toCodeL1 L1.OP_WITHIN
opcodeL2ToCodeL1 OP_RIPEMD160 = Just $ toCodeL1 L1.OP_RIPEMD160
opcodeL2ToCodeL1 OP_SHA1 = Just $ toCodeL1 L1.OP_SHA1
opcodeL2ToCodeL1 OP_SHA256 = Just $ toCodeL1 L1.OP_SHA256
opcodeL2ToCodeL1 OP_HASH160 = Just $ toCodeL1 L1.OP_HASH160
opcodeL2ToCodeL1 OP_HASH256 = Just $ toCodeL1 L1.OP_HASH256
opcodeL2ToCodeL1 OP_CODESEPARATOR = Just $ toCodeL1 L1.OP_CODESEPARATOR
opcodeL2ToCodeL1 OP_CHECKSIG = Just $ toCodeL1 L1.OP_CHECKSIG
opcodeL2ToCodeL1 OP_CHECKSIGVERIFY = Just $ toCodeL1 L1.OP_CHECKSIGVERIFY
opcodeL2ToCodeL1 OP_CHECKMULTISIG = Just $ toCodeL1 L1.OP_CHECKMULTISIG
opcodeL2ToCodeL1 OP_CHECKMULTISIGVERIFY =
  Just $ toCodeL1 L1.OP_CHECKMULTISIGVERIFY
opcodeL2ToCodeL1 OP_CHECKLOCKTIMEVERIFY =
  Just $ toCodeL1 L1.OP_CHECKLOCKTIMEVERIFY
opcodeL2ToCodeL1 OP_CHECKSEQUENCEVERIFY =
  Just $ toCodeL1 L1.OP_CHECKSEQUENCEVERIFY
opcodeL2ToCodeL1 OP_CHECKDATASIG = Just $ toCodeL1 L1.OP_CHECKDATASIG
opcodeL2ToCodeL1 OP_CHECKDATASIGVERIFY =
  Just $ toCodeL1 L1.OP_CHECKDATASIGVERIFY
opcodeL2ToCodeL1 OP_REVERSEBYTES = Just $ toCodeL1 L1.OP_REVERSEBYTES
opcodeL2ToCodeL1 OP_INPUTINDEX = Just $ toCodeL1 L1.OP_INPUTINDEX
opcodeL2ToCodeL1 OP_ACTIVEBYTECODE = Just $ toCodeL1 L1.OP_ACTIVEBYTECODE
opcodeL2ToCodeL1 OP_TXVERSION = Just $ toCodeL1 L1.OP_TXVERSION
opcodeL2ToCodeL1 OP_TXINPUTCOUNT = Just $ toCodeL1 L1.OP_TXINPUTCOUNT
opcodeL2ToCodeL1 OP_TXOUTPUTCOUNT = Just $ toCodeL1 L1.OP_TXOUTPUTCOUNT
opcodeL2ToCodeL1 OP_TXLOCKTIME = Just $ toCodeL1 L1.OP_TXLOCKTIME
opcodeL2ToCodeL1 OP_UTXOVALUE = Just $ toCodeL1 L1.OP_UTXOVALUE
opcodeL2ToCodeL1 OP_UTXOBYTECODE = Just $ toCodeL1 L1.OP_UTXOBYTECODE
opcodeL2ToCodeL1 OP_OUTPOINTTXHASH = Just $ toCodeL1 L1.OP_OUTPOINTTXHASH
opcodeL2ToCodeL1 OP_OUTPOINTINDEX = Just $ toCodeL1 L1.OP_OUTPOINTINDEX
opcodeL2ToCodeL1 OP_INPUTBYTECODE = Just $ toCodeL1 L1.OP_INPUTBYTECODE
opcodeL2ToCodeL1 OP_INPUTSEQUENCENUMBER =
  Just $ toCodeL1 L1.OP_INPUTSEQUENCENUMBER
opcodeL2ToCodeL1 OP_OUTPUTVALUE = Just $ toCodeL1 L1.OP_OUTPUTVALUE
opcodeL2ToCodeL1 OP_OUTPUTBYTECODE = Just $ toCodeL1 L1.OP_OUTPUTBYTECODE
opcodeL2ToCodeL1 OP_UTXOTOKENCATEGORY = Just $ toCodeL1 L1.OP_UTXOTOKENCATEGORY
opcodeL2ToCodeL1 OP_UTXOTOKENCOMMITMENT =
  Just $ toCodeL1 L1.OP_UTXOTOKENCOMMITMENT
opcodeL2ToCodeL1 OP_UTXOTOKENAMOUNT = Just $ toCodeL1 L1.OP_UTXOTOKENAMOUNT
opcodeL2ToCodeL1 OP_OUTPUTTOKENCATEGORY =
  Just $ toCodeL1 L1.OP_OUTPUTTOKENCATEGORY
opcodeL2ToCodeL1 OP_OUTPUTTOKENCOMMITMENT =
  Just $ toCodeL1 L1.OP_OUTPUTTOKENCOMMITMENT
opcodeL2ToCodeL1 OP_OUTPUTTOKENAMOUNT = Just $ toCodeL1 L1.OP_OUTPUTTOKENAMOUNT
opcodeL2ToCodeL1 (OP_UNUSED opcodeL1) = Just $ toCodeL1 opcodeL1

toCodeL1 :: OpcodeL1 -> CodeL1
toCodeL1 = B.singleton . fromIntegral . fromEnum

getOp :: CodeL1 -> Maybe (OpcodeL2, CodeL1)
getOp code | B.null code = Nothing
getOp code =
  case B.uncons code of
    Just (op, code') -> getOp' (L1.word8ToOpcodeL1 op) code'
    Nothing -> canNotHappen

getOp' :: OpcodeL1 -> CodeL1 -> Maybe (OpcodeL2, CodeL1)
getOp' L1.OP_0 code = Just (OP_0, code)
getOp' L1.OP_PUSHDATA1 code = do
  guard (B.length code >= 1)
  let (countBs, code') = B.splitAt 1 code
      count = fromIntegral $ B.head countBs
  (pushVal, rest) <- getPushVal count code'
  Just (OP_DATA L1.OP_PUSHDATA1 pushVal, rest)
getOp' L1.OP_PUSHDATA2 code = do
  guard (B.length code >= 2)
  let (countBs, code') = B.splitAt 2 code
      count = fromIntegral $ runGet getWord16le (B.fromStrict countBs)
  (pushVal, rest) <- getPushVal count code'
  Just (OP_DATA L1.OP_PUSHDATA2 pushVal, rest)
getOp' L1.OP_PUSHDATA4 code = do
  guard (B.length code >= 4)
  let (countBs, code') = B.splitAt 4 code
      count = fromIntegral $ runGet getWord32le (B.fromStrict countBs)
  (pushVal, rest) <- getPushVal count code'
  Just (OP_DATA L1.OP_PUSHDATA4 pushVal, rest)
getOp' L1.OP_1NEGATE code = Just (OP_1NEGATE, code)
getOp' op@L1.OP_RESERVED code = Just (OP_UNUSED op, code)
getOp' L1.OP_1 code = Just (OP_1, code)
getOp' L1.OP_2 code = Just (OP_2, code)
getOp' L1.OP_3 code = Just (OP_3, code)
getOp' L1.OP_4 code = Just (OP_4, code)
getOp' L1.OP_5 code = Just (OP_5, code)
getOp' L1.OP_6 code = Just (OP_6, code)
getOp' L1.OP_7 code = Just (OP_7, code)
getOp' L1.OP_8 code = Just (OP_8, code)
getOp' L1.OP_9 code = Just (OP_9, code)
getOp' L1.OP_10 code = Just (OP_10, code)
getOp' L1.OP_11 code = Just (OP_11, code)
getOp' L1.OP_12 code = Just (OP_12, code)
getOp' L1.OP_13 code = Just (OP_13, code)
getOp' L1.OP_14 code = Just (OP_14, code)
getOp' L1.OP_15 code = Just (OP_15, code)
getOp' L1.OP_16 code = Just (OP_16, code)
getOp' L1.OP_NOP code = Just (OP_NOP, code)
getOp' L1.OP_VER_OP_EVAL code = Just (OP_EVAL, code)
getOp' L1.OP_IF code = Just (OP_IF, code)
getOp' L1.OP_NOTIF code = Just (OP_NOTIF, code)
getOp' op@L1.OP_VERIF code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_VERNOTIF code = Just (OP_UNUSED op, code)
getOp' L1.OP_ELSE code = Just (OP_ELSE, code)
getOp' L1.OP_ENDIF code = Just (OP_ENDIF, code)
getOp' L1.OP_VERIFY code = Just (OP_VERIFY, code)
getOp' L1.OP_RETURN code = Just (OP_RETURN, code)
getOp' L1.OP_TOALTSTACK code = Just (OP_TOALTSTACK, code)
getOp' L1.OP_FROMALTSTACK code = Just (OP_FROMALTSTACK, code)
getOp' L1.OP_2DROP code = Just (OP_2DROP, code)
getOp' L1.OP_2DUP code = Just (OP_2DUP, code)
getOp' L1.OP_3DUP code = Just (OP_3DUP, code)
getOp' L1.OP_2OVER code = Just (OP_2OVER, code)
getOp' L1.OP_2ROT code = Just (OP_2ROT, code)
getOp' L1.OP_2SWAP code = Just (OP_2SWAP, code)
getOp' L1.OP_IFDUP code = Just (OP_IFDUP, code)
getOp' L1.OP_DEPTH code = Just (OP_DEPTH, code)
getOp' L1.OP_DROP code = Just (OP_DROP, code)
getOp' L1.OP_DUP code = Just (OP_DUP, code)
getOp' L1.OP_NIP code = Just (OP_NIP, code)
getOp' L1.OP_OVER code = Just (OP_OVER, code)
getOp' L1.OP_PICK code = Just (OP_PICK, code)
getOp' L1.OP_ROLL code = Just (OP_ROLL, code)
getOp' L1.OP_ROT code = Just (OP_ROT, code)
getOp' L1.OP_SWAP code = Just (OP_SWAP, code)
getOp' L1.OP_TUCK code = Just (OP_TUCK, code)
getOp' L1.OP_CAT code = Just (OP_CAT, code)
getOp' L1.OP_SPLIT code = Just (OP_SPLIT, code)
getOp' L1.OP_NUM2BIN code = Just (OP_NUM2BIN, code)
getOp' L1.OP_BIN2NUM code = Just (OP_BIN2NUM, code)
getOp' L1.OP_SIZE code = Just (OP_SIZE, code)
getOp' op@L1.OP_INVERT code = Just (OP_UNUSED op, code)
getOp' L1.OP_AND code = Just (OP_AND, code)
getOp' L1.OP_OR code = Just (OP_OR, code)
getOp' L1.OP_XOR code = Just (OP_XOR, code)
getOp' L1.OP_EQUAL code = Just (OP_EQUAL, code)
getOp' L1.OP_EQUALVERIFY code = Just (OP_EQUALVERIFY, code)
getOp' op@L1.OP_RESERVED1 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_RESERVED2 code = Just (OP_UNUSED op, code)
getOp' L1.OP_1ADD code = Just (OP_1ADD, code)
getOp' L1.OP_1SUB code = Just (OP_1SUB, code)
getOp' op@L1.OP_2MUL code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_2DIV code = Just (OP_UNUSED op, code)
getOp' L1.OP_NEGATE code = Just (OP_NEGATE, code)
getOp' L1.OP_ABS code = Just (OP_ABS, code)
getOp' L1.OP_NOT code = Just (OP_NOT, code)
getOp' L1.OP_0NOTEQUAL code = Just (OP_0NOTEQUAL, code)
getOp' L1.OP_ADD code = Just (OP_ADD, code)
getOp' L1.OP_SUB code = Just (OP_SUB, code)
getOp' L1.OP_MUL code = Just (OP_MUL, code)
getOp' L1.OP_DIV code = Just (OP_DIV, code)
getOp' L1.OP_MOD code = Just (OP_MOD, code)
getOp' op@L1.OP_LSHIFT code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_RSHIFT code = Just (OP_UNUSED op, code)
getOp' L1.OP_BOOLAND code = Just (OP_BOOLAND, code)
getOp' L1.OP_BOOLOR code = Just (OP_BOOLOR, code)
getOp' L1.OP_NUMEQUAL code = Just (OP_NUMEQUAL, code)
getOp' L1.OP_NUMEQUALVERIFY code = Just (OP_NUMEQUALVERIFY, code)
getOp' L1.OP_NUMNOTEQUAL code = Just (OP_NUMNOTEQUAL, code)
getOp' L1.OP_LESSTHAN code = Just (OP_LESSTHAN, code)
getOp' L1.OP_GREATERTHAN code = Just (OP_GREATERTHAN, code)
getOp' L1.OP_LESSTHANOREQUAL code = Just (OP_LESSTHANOREQUAL, code)
getOp' L1.OP_GREATERTHANOREQUAL code = Just (OP_GREATERTHANOREQUAL, code)
getOp' L1.OP_MIN code = Just (OP_MIN, code)
getOp' L1.OP_MAX code = Just (OP_MAX, code)
getOp' L1.OP_WITHIN code = Just (OP_WITHIN, code)
getOp' L1.OP_RIPEMD160 code = Just (OP_RIPEMD160, code)
getOp' L1.OP_SHA1 code = Just (OP_SHA1, code)
getOp' L1.OP_SHA256 code = Just (OP_SHA256, code)
getOp' L1.OP_HASH160 code = Just (OP_HASH160, code)
getOp' L1.OP_HASH256 code = Just (OP_HASH256, code)
getOp' L1.OP_CODESEPARATOR code = Just (OP_CODESEPARATOR, code)
getOp' L1.OP_CHECKSIG code = Just (OP_CHECKSIG, code)
getOp' L1.OP_CHECKSIGVERIFY code = Just (OP_CHECKSIGVERIFY, code)
getOp' L1.OP_CHECKMULTISIG code = Just (OP_CHECKMULTISIG, code)
getOp' L1.OP_CHECKMULTISIGVERIFY code = Just (OP_CHECKMULTISIGVERIFY, code)
getOp' op@L1.OP_NOP1 code = Just (OP_UNUSED op, code)
getOp' L1.OP_CHECKLOCKTIMEVERIFY code = Just (OP_CHECKLOCKTIMEVERIFY, code)
getOp' L1.OP_CHECKSEQUENCEVERIFY code = Just (OP_CHECKSEQUENCEVERIFY, code)
getOp' op@L1.OP_NOP4 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_NOP5 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_NOP6 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_NOP7 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_NOP8 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_NOP9 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_NOP10 code = Just (OP_UNUSED op, code)
getOp' L1.OP_CHECKDATASIG code = Just (OP_CHECKDATASIG, code)
getOp' L1.OP_CHECKDATASIGVERIFY code = Just (OP_CHECKDATASIGVERIFY, code)
getOp' L1.OP_REVERSEBYTES code = Just (OP_REVERSEBYTES, code)
getOp' op@L1.OP_AVAILABLE_BD code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_AVAILABLE_BE code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_AVAILABLE_BF code = Just (OP_UNUSED op, code)
getOp' L1.OP_INPUTINDEX code = Just (OP_INPUTINDEX, code)
getOp' L1.OP_ACTIVEBYTECODE code = Just (OP_ACTIVEBYTECODE, code)
getOp' L1.OP_TXVERSION code = Just (OP_TXVERSION, code)
getOp' L1.OP_TXINPUTCOUNT code = Just (OP_TXINPUTCOUNT, code)
getOp' L1.OP_TXOUTPUTCOUNT code = Just (OP_TXOUTPUTCOUNT, code)
getOp' L1.OP_TXLOCKTIME code = Just (OP_TXLOCKTIME, code)
getOp' L1.OP_UTXOVALUE code = Just (OP_UTXOVALUE, code)
getOp' L1.OP_UTXOBYTECODE code = Just (OP_UTXOBYTECODE, code)
getOp' L1.OP_OUTPOINTTXHASH code = Just (OP_OUTPOINTTXHASH, code)
getOp' L1.OP_OUTPOINTINDEX code = Just (OP_OUTPOINTINDEX, code)
getOp' L1.OP_INPUTBYTECODE code = Just (OP_INPUTBYTECODE, code)
getOp' L1.OP_INPUTSEQUENCENUMBER code = Just (OP_INPUTSEQUENCENUMBER, code)
getOp' L1.OP_OUTPUTVALUE code = Just (OP_OUTPUTVALUE, code)
getOp' L1.OP_OUTPUTBYTECODE code = Just (OP_OUTPUTBYTECODE, code)
getOp' L1.OP_UTXOTOKENCATEGORY code = Just (OP_UTXOTOKENCATEGORY, code)
getOp' L1.OP_UTXOTOKENCOMMITMENT code = Just (OP_UTXOTOKENCOMMITMENT, code)
getOp' L1.OP_UTXOTOKENAMOUNT code = Just (OP_UTXOTOKENAMOUNT, code)
getOp' L1.OP_OUTPUTTOKENCATEGORY code = Just (OP_OUTPUTTOKENCATEGORY, code)
getOp' L1.OP_OUTPUTTOKENCOMMITMENT code = Just (OP_OUTPUTTOKENCOMMITMENT, code)
getOp' L1.OP_OUTPUTTOKENAMOUNT code = Just (OP_OUTPUTTOKENAMOUNT, code)
getOp' op@L1.OP_RESERVED3 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_RESERVED4 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_D6 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_D7 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_D8 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_D9 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_DA code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_DB code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_DC code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_DD code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_DE code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_DF code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E0 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E1 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E2 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E3 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E4 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E5 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E6 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E7 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E8 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_E9 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_EA code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_EB code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_EC code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_ED code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_EE code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_EF code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F0 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F1 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F2 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F3 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F4 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F5 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F6 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F7 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F8 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_F9 code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_FA code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_FB code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_FC code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_FD code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_FE code = Just (OP_UNUSED op, code)
getOp' op@L1.OP_UNASSIGNED_FF code = Just (OP_UNUSED op, code)
getOp' opcodeL1 code =
  if opcodeL1 >= L1.OP_DATA_01 && opcodeL1 <= L1.OP_DATA_75
    then first (OP_DATA opcodeL1) <$> getPushVal (fromEnum opcodeL1) code
    else canNotHappen

getPushVal :: Int -> CodeL1 -> Maybe (Bytes, CodeL1)
getPushVal count code | count <= B.length code = Just $ B.splitAt count code
getPushVal _count _code = Nothing

{- ORMOLU_DISABLE -}
bytesToDataOp :: Bytes -> OpcodeL2
bytesToDataOp bytes =
  case B.length bytes of
    0 -> OP_0
    1 -> case B.head bytes of
      1 -> OP_1; 2 -> OP_2; 3 -> OP_3; 4 -> OP_4; 5 -> OP_5; 6 -> OP_6;
      7 -> OP_7; 8 -> OP_8; 9 -> OP_9; 10 -> OP_10; 11 -> OP_11; 12 -> OP_12;
      13 -> OP_13; 14 -> OP_14; 15 -> OP_15; 16 -> OP_16
      0x81 -> OP_1NEGATE
      _ -> OP_DATA L1.OP_DATA_01 bytes
    x | x > 1 && x <= 75 -> OP_DATA (toEnum x) bytes
    x | x <= fromIntegral (maxBound :: Word8) -> OP_DATA L1.OP_PUSHDATA1 bytes
    x | x <= fromIntegral (maxBound :: Word16) -> OP_DATA L1.OP_PUSHDATA2 bytes
    x | x <= fromIntegral (maxBound :: Word32) -> OP_DATA L1.OP_PUSHDATA4 bytes
    _ -> error "bytesToDataOp: ByteString too long."
{- ORMOLU_ENABLE -}

isMinimal :: OpcodeL2 -> Bool
isMinimal (OP_DATA L1.OP_DATA_01 bytes) =
  let byte = B.head bytes
   in not ((byte >= 1 && byte <= 16) || byte == minusOne)
  where
    minusOne = 0x81
isMinimal (OP_DATA L1.OP_PUSHDATA1 bytes) =
  B.length bytes > fromEnum L1.OP_DATA_75
isMinimal (OP_DATA L1.OP_PUSHDATA2 bytes) = B.length bytes > 0xff
isMinimal (OP_DATA L1.OP_PUSHDATA4 bytes) = B.length bytes > 0xffff
isMinimal _ = True
