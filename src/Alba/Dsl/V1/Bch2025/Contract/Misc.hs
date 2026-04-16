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

import Alba.Dsl.V1.Bch2025
  ( Fn,
    FnA,
    Stack (..),
    StackNum,
    TBool,
    TBytes,
    TNat,
    bytes,
    i2n,
    i2nUnsafe,
    n2i,
    op0,
    op1Sub,
    opEqual,
    opIf,
    opNumEqual,
    opSub,
    opSwap,
    (.),
  )
import Prelude ()

natSub :: Fn (s :> TNat :> TNat) (s :> TNat)
natSub = n2i . opSwap . n2i . opSwap . opSub . i2n

natSubUnsafe :: Fn (s :> TNat :> TNat) (s :> TNat)
natSubUnsafe = n2i . opSwap . n2i . opSwap . opSub . i2nUnsafe

nat1Sub :: Fn (s :> TNat) (s :> TNat)
nat1Sub = n2i . op1Sub . i2n

nat1SubUnsafe :: Fn (s :> TNat) (s :> TNat)
nat1SubUnsafe = n2i . op1Sub . i2nUnsafe

ifZero ::
  (StackNum x1) =>
  FnA s alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s :> x1) alt s' alt'
ifZero ifOps elseOps = isZero . opIf ifOps elseOps

isZero :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
isZero = op0 . opNumEqual

null :: Fn (s :> TBytes) (s :> TBool)
null = bytes [] . opEqual
