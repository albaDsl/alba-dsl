-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.CompilerUtils (pushIntegerCode, aop, aops) where

import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..), bytesToDataOp)
import Alba.Vm.Common.VmInteger (integerToBytes)
import Data.Sequence qualified as S

-- Add op. Adds an operation to the end of the code.
aop :: CodeL2 -> OpcodeL2 -> CodeL2
aop = (S.:|>)

-- Add ops. Adds several operations to the end of the code.
aops :: CodeL2 -> [OpcodeL2] -> CodeL2
aops code ops = code <> S.fromList ops

pushIntegerCode :: Integer -> CodeL2
pushIntegerCode n = S.singleton $ bytesToDataOp (integerToBytes n)
