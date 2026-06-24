module Alba.Dsl.V1.Bch2026.Contract.Math
  ( isEven,
    isOdd,
    square,
    halve,
    pow,
    pow',
    factorial,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    Stack ((:>)),
    StackNum,
    TBool,
    TInt,
    TNat,
    fn,
    int,
    name,
    nat,
    ns2,
    ns3,
    op0,
    op1,
    op2,
    op2Drop,
    opDiv,
    opDup,
    opMod,
    opMul,
    opNumEqual,
    opUntil,
    opWhen,
    pick,
    roll,
    un,
  )
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Misc
  ( ifZero,
    isZero,
    nat1SubUnsafe,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip, swap)
import Alba.Dsl.V1.Common.Lang (begin, (.))
import Prelude ()

isEven :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
isEven = op2 . opMod . op0 . opNumEqual

isOdd :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
isOdd = op2 . opMod . op1 . opNumEqual

square :: (StackNum x1) => Fn (s :> x1) (s :> x1)
square = opDup . opMul

halve :: (StackNum x1) => Fn (s :> x1) (s :> x1)
halve = op2 . opDiv

-- >>> import Alba.Dsl.V1.Bch2026
-- >>> progSize pow
-- "2 opcodes, 2 bytes. Total (with function table): 5 opcodes, 39 bytes.\n"
pow :: Fn (s :> TInt :> TNat) (s :> TInt)
pow = fn (pow' mul)

-- Macro for calculating pow. The multiplication operator to use is provided as
-- an argument.
pow' ::
  (forall s'. Fn (s' :> TInt :> TInt) (s' :> TInt)) ->
  Fn (s :> TInt :> TNat) (s :> TInt)
pow' f = dup . ifZero (op2Drop . int 1) (int 1 . opUntil loop . nip . nip)
  where
    loop :: Loop (s :> TInt :> TNat :> TInt)
    loop =
      begin
        . ns3 #b #n #res
        . (roll #res . pick #n . isOdd . opWhen (pick #b . f))
        . (roll #b . square' . swap)
        . (name #n' (roll #n . halve) . swap)
        . (pick #n' . isZero . un #n')

    square' :: Fn (s :> TInt) (s :> TInt)
    square' = dup . f

factorial :: Fn (s :> TNat) (s :> TNat)
factorial = dup . ifZero (drop . nat 1) (nat 1 . swap . opUntil loop . drop)
  where
    loop :: Loop (s :> TNat :> TNat)
    loop =
      begin
        . (ns2 #product #n . roll #product . pick #n . mul)
        . (roll #n . nat1SubUnsafe . dup . isZero)
