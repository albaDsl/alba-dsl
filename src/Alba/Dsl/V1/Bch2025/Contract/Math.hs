module Alba.Dsl.V1.Bch2025.Contract.Math
  ( isEven,
    isOdd,
    square,
    halve,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Fn,
    Stack (..),
    StackNum,
    TBool,
    op0,
    op1,
    op2,
    opDiv,
    opDup,
    opMod,
    opMul,
    opNumEqual,
    (.),
  )
import Prelude ()

isEven :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
isEven = op2 . opMod . op0 . opNumEqual

isOdd :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
isOdd = op2 . opMod . op1 . opNumEqual

square :: (StackNum x1) => Fn (s :> x1) (s :> x1)
square = opDup . opMul

halve :: (StackNum x1) => Fn (s :> x1) (s :> x1)
halve = op2 . opDiv
