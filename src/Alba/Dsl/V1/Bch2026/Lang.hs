-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang (lambda, lambda', recur) where

import Alba.Dsl.V1.Bch2025.Compile (Optimize (None), compile)
import Alba.Dsl.V1.Bch2025.CompilerUtils (aop, aops)
import Alba.Dsl.V1.Bch2026.Stack (TLambda, TLambdaUntyped)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..), bytesToDataOp)

lambda ::
  FNA s alt s' alt' ->
  FN s'' (s'' > TLambda (S s alt -> S s' alt'))
lambda p (S c) = S (aop c (bytesToDataOp (compile None p)))

lambda' :: FNA s alt s' alt' -> FN s'' (s'' > TLambdaUntyped)
lambda' p (S c) = S (aop c (bytesToDataOp (compile None p)))

recur ::
  FNA (s > TLambdaUntyped) alt s' alt' ->
  FNA (s > TLambdaUntyped) alt s' alt'
recur _p (S c) = S (aops c [OP_DUP, OP_EVAL])
