-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Ops where

import Alba.Dsl.V1.Bch2025.CompilerUtils (aop)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (S (S))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

opEval :: S (s > TLambda (S s alt -> S s' alt')) alt -> S s' alt'
opEval (S c) = S (aop c OP_EVAL)
