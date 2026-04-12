-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.OpsUntyped where

import Alba.Dsl.V1.Common.CompilerUtilsUntyped (aop)
import Alba.Dsl.V1.Common.StackUntyped (FnU)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Control.Arrow ((>>>))

opUntil :: FnU -> FnU
opUntil loopBody = aop OP_BEGIN >>> loopBody >>> aop OP_UNTIL

opDefine :: FnU
opDefine = aop OP_DEFINE

opInvoke :: FnU
opInvoke = aop OP_INVOKE
