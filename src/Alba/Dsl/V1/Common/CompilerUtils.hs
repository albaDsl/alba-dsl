-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.CompilerUtils
  ( pushIntegerOp,
    aop,
    aop',
    aops,
    aops',
  )
where

import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, OpcodeL3 (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..), bytesToDataOp)
import Alba.Vm.Common.VmInteger (integerToBytes)
import Data.Sequence qualified as S

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

pushIntegerOp :: Integer -> OpcodeL2
pushIntegerOp n = bytesToDataOp (integerToBytes n)
