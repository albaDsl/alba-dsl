-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.CompilerUtilsUntyped (aop, aop', aops, aops') where

import Alba.Dsl.V1.Common.OpcodeL3 (OpcodeL3 (..))
import Alba.Dsl.V1.Common.StackUntyped (SU (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Sequence qualified as S

-- Add op. Adds an operation to the end of the code.
aop :: OpcodeL2 -> SU -> SU
aop op st = st {c = st.c S.:|> Opcode op}

aop' :: OpcodeL3 -> SU -> SU
aop' op st = st {c = st.c S.:|> op}

-- Add ops. Adds several operations to the end of the code.
aops :: [OpcodeL2] -> SU -> SU
aops ops st = st {c = st.c <> (Opcode <$> S.fromList ops)}

aops' :: [OpcodeL3] -> SU -> SU
aops' ops st = st {c = st.c <> (S.fromList ops)}
