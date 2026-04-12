-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2025.Contract.Misc
  ( natSub,
    natSubUnsafe,
    ifZero,
    isZero,
    null,
    nat1Sub,
    nat1SubUnsafe,
  )
where

import Alba.Dsl.V1.Bch2025.Conversion (i2n, i2nUnsafe, n2i)
import Alba.Dsl.V1.Bch2025.Lang (bytes)
import Alba.Dsl.V1.Bch2025.Ops
  ( op0,
    op1Sub,
    opEqual,
    opIf,
    opNumEqual,
    opSub,
    opSwap,
  )
import Alba.Dsl.V1.Bch2025.Stack (StackNum)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.Stack (Fn, FnA, TBool, TBytes, TNat)
import Prelude hiding (null)

natSub :: Fn (s > TNat > TNat) (s > TNat)
natSub = n2i # opSwap # n2i # opSwap # opSub # i2n

natSubUnsafe :: Fn (s > TNat > TNat) (s > TNat)
natSubUnsafe = n2i # opSwap # n2i # opSwap # opSub # i2nUnsafe

nat1Sub :: Fn (s > TNat) (s > TNat)
nat1Sub = n2i # op1Sub # i2n

nat1SubUnsafe :: Fn (s > TNat) (s > TNat)
nat1SubUnsafe = n2i # op1Sub # i2nUnsafe

ifZero ::
  (StackNum x1) =>
  FnA s alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s > x1) alt s' alt'
ifZero ifOps elseOps = isZero # opIf ifOps elseOps

isZero :: (StackNum x1) => Fn (s > x1) (s > TBool)
isZero = op0 # opNumEqual

null :: Fn (s > TBytes) (s > TBool)
null = bytes [] # opEqual
