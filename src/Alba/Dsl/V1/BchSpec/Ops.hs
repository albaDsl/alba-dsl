-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.BchSpec.Ops where

import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.Stack (FnA, Stack (..), TCode)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

opEval :: FnA s alt s' alt' -> FnA (s :> TCode) alt s' alt'
opEval _prog = aop OP_EVAL
