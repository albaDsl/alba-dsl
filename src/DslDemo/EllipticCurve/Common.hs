-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Common (doubleN, mods, countTrailingZeros) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    TInt,
    TNat,
    fn,
    i2nUnsafe,
    int,
    nat,
    ns,
    op0,
    opFalse,
    opIf,
    opRShiftNum,
    opTrue,
    opUntil,
    pick,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( Integral (add1, mod),
    Ord (..),
    dup,
    equal,
    ifZero,
    isEven,
    mod,
    nip,
    over,
    sub,
    sub1,
    swap,
  )
import Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances ()
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ)
import Prelude (Int, (-), (^))

doubleN :: Fn (s :> TNat :> TPointJ) (s :> TPointJ)
doubleN =
  fn
    ( opUntil
        ( (swap . dup . op0 . equal)
            . opIf (swap . opTrue) (sub1 . swap . EC.ecDoubleJ . opFalse)
        )
        . nip
    )

mods :: Int -> Fn (s :> TInt) (s :> TInt)
mods windowSize =
  (full . mod . ns #r . pick #r . half . greaterThanOrEqual)
    . (opIf (roll #r . full . sub) (roll #r))
  where
    full = int (2 ^ windowSize)
    half = int (2 ^ (windowSize - 1))

countTrailingZeros :: Fn (s :> TInt) (s :> TNat)
countTrailingZeros =
  fn
    ( dup
        . (ifZero i2nUnsafe)
          ( nat 0
              . opUntil
                ( (over . isEven)
                    . opIf
                      (add1 . swap . nat 1 . opRShiftNum . swap . opFalse)
                      opTrue
                )
              . nip
          )
    )
