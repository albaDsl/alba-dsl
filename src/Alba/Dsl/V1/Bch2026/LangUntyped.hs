-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.LangUntyped
  ( bytes,
    int,
    cond,
    concatProg,
    repeatProg,
  )
where

import Alba.Dsl.V1.Bch2026.OpsUntyped (opDup, opIf)
import Alba.Dsl.V1.Common.CompilerUtils (bytesToDataOp, integerToDataOp)
import Alba.Dsl.V1.Common.CompilerUtilsUntyped (aop)
import Alba.Dsl.V1.Common.StackUntyped (FnU, SU, (∘))
import Alba.Vm.Common.BasicTypes (Bytes)

int :: Integer -> FnU
int n = aop (integerToDataOp n)

bytes :: Bytes -> FnU
bytes x = aop (bytesToDataOp x)

cond :: [(SU -> SU, SU -> SU)] -> FnU -> FnU
cond [] def st = def st
cond ((test, result) : rest) def st =
  (opDup ∘ test ∘ opIf result (cond rest def)) st

repeatProg :: Int -> FnU -> FnU
repeatProg count prog st = iterate prog st !! count

concatProg :: FnU -> FnU -> FnU
concatProg p1 p2 st =
  let st' = p1 st
      st'' = p2 st'
   in st''
