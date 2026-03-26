-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.OpsUntyped where

import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.StackUntyped (FNU, SU (SU))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

opUntil :: FNU -> FNU
opUntil loopBody (SU c fs) =
  let (SU c' fs') = loopBody (SU (aop c OP_BEGIN) fs)
   in SU (aop c' OP_UNTIL) fs'

opDefine :: FNU
opDefine (SU c fs) = SU (aop c OP_DEFINE) fs

opInvoke :: FNU
opInvoke (SU c fs) = SU (aop c OP_INVOKE) fs
