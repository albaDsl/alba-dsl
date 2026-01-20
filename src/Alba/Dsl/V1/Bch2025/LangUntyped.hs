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
import Alba.Dsl.V1.Common.CompilerUtils (aop, bytesToDataOp, pushIntegerOp)
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, SU (SU))
import Alba.Vm.Common.BasicTypes (Bytes)

int :: Integer -> FNU
int n (SU c fs) = SU (aop c (pushIntegerOp (fromIntegral n))) fs

bytes :: Bytes -> FNU
bytes x (SU c fs) = SU (aop c (bytesToDataOp x)) fs

cond :: [(SU -> SU, SU -> SU)] -> FNU -> FNU
cond [] def st = def st
cond ((test, result) : rest) def st =
  (opDup # test # opIf result (cond rest def)) st

repeatProg :: Int -> FNU -> FNU
repeatProg count prog st = iterate prog st !! count

concatProg :: FNU -> FNU -> FNU
concatProg p1 p2 st =
  let st' = p1 st
      st'' = p2 st'
   in st''
