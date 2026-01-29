-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.CompilerUtils
  ( aop,
    aop',
    aops,
    aops',
    integerToDataOp,
    bytesToDataOp,
  )
where

import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, OpcodeL3 (..))
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.VmInteger (integerToBytes)
import Data.ByteString qualified as B
import Data.Sequence qualified as S
import Data.Word (Word16, Word32, Word8)

-- Add op. Adds an operation to the end of the code.
aop :: CodeL3 -> OpcodeL2 -> CodeL3
aop ops op = ops S.:|> Opcode op

aop' :: CodeL3 -> OpcodeL3 -> CodeL3
aop' ops op = ops S.:|> op

-- Add ops. Adds several operations to the end of the code.
aops :: CodeL3 -> [OpcodeL2] -> CodeL3
aops code ops = code <> (Opcode <$> S.fromList ops)

aops' :: CodeL3 -> [OpcodeL3] -> CodeL3
aops' code ops = code <> S.fromList ops

integerToDataOp :: Integer -> OpcodeL2
integerToDataOp n = bytesToDataOp (integerToBytes n)

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
