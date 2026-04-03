-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.LangUntyped
  ( bytes,
    int,
    cond,
    concatProg,
    repeatProg,
  )
where

import Alba.Dsl.V1.Bch2025.OpsUntyped (opDup, opIf)
import Alba.Dsl.V1.Common.CompilerUtils (aop, bytesToDataOp, integerToDataOp)
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.StackUntyped (FnU, SU (SU))
import Alba.Vm.Common.BasicTypes (Bytes)

int :: Integer -> FnU
int n (SU c fs) = SU (aop c (integerToDataOp (fromIntegral n))) fs

bytes :: Bytes -> FnU
bytes x (SU c fs) = SU (aop c (bytesToDataOp x)) fs

cond :: [(SU -> SU, SU -> SU)] -> FnU -> FnU
cond [] def st = def st
cond ((test, result) : rest) def st =
  (opDup # test # opIf result (cond rest def)) st

repeatProg :: Int -> FnU -> FnU
repeatProg count prog st = iterate prog st !! count

concatProg :: FnU -> FnU -> FnU
concatProg p1 p2 st =
  let st' = p1 st
      st'' = p2 st'
   in st''
