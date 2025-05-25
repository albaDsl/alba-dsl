module RecursionExamples.EllipticCurveField
  ( feAdd,
    feSub,
    feMul,
    feSquare,
    feInv,
  )
where

import Alba.Dsl.V1.Bch2026
import RecursionExamples.EllipticCurveConstants (p)
import RecursionExamples.Exponentiation (pow')

feAdd :: FN (s > TInt > TInt) (s > TInt)
feAdd = opAdd # int (fromIntegral p) # opMod

feSub :: FN (s > TInt > TInt) (s > TInt)
feSub = opSub # int (fromIntegral p) # unname @2 modulo

feMul :: FN (s > TInt > TInt) (s > TInt)
feMul = opMul # int (fromIntegral p) # opMod

feSquare :: FN (s > TInt) (s > TInt)
feSquare = opDup # feMul

feInv :: FN (s > TInt) (s > TInt)
feInv = nat (p - 2) # pow' feMul

modulo :: FN (s > N "x1" TInt > N "x2" TInt) (s > TInt)
modulo =
  begin
    # argPick @"x1"
    # argPick @"x2"
    # name @"res" opMod
    # argPick @"res"
    # int 0
    # opLessThan
    # opIf
      (argPick @"res" # argPick @"x2" # opAdd # argsDrop @3)
      (argPick @"res" # argsDrop @3)
