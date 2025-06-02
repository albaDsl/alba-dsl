-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Ops where

import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FNA, S (S), TBool)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

opUntil :: FNA s alt (s > TBool) alt -> FNA s alt s alt
opUntil loopBody (S c) =
  let (S c') = loopBody (S (aop c OP_BEGIN))
   in S (aop c' OP_UNTIL)

opEval :: S (s > TLambda (S s alt -> S s' alt')) alt -> S s' alt'
opEval (S c) = S (aop c OP_EVAL)
