module DslDemo.EllipticCurve.EllipticCurvePackedNonRecursive
  ( ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Math (half, isOdd)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField (TPrimeModulus, primeModulus)
import DslDemo.EllipticCurve.EllipticCurvePackedCommon (ecAdd, ecDouble)
import DslDemo.EllipticCurve.EllipticCurvePoint (TPoint, makeIdentity)

type LoopTypeN s =
  s
    > N "n" TNat
    > N "p" TPoint
    > N "r" TPoint
    > N "pmod" TPrimeModulus

type LoopType s = s > TNat > TPoint > TPoint > TPrimeModulus

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = unname @2 ecMul'
  where
    ecMul' :: FN (s > N "n" TNat > N "p" TPoint) (s > TPoint)
    ecMul' =
      begin
        # argPick @"n"
        # (nat 1 # opNumEqual)
        # opIf
          (argRoll @"p" # argsDrop @1)
          ( begin
              # argRoll @"n"
              # argRoll @"p"
              # makeIdentity
              # primeModulus
              # opUntil (unname @4 loop)
              # opDrop
              # opNip
              # opNip
          )

    loop :: FN (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # name @"r2"
          ( begin
              # argRoll @"r"
              # ex1 (argPick @"n" # isOdd)
              # opWhen (argPick @"p" # argPick @"pmod" # ecAdd)
          )
        # (argPick @"n" # half)
        # (argRoll @"p" # argPick @"pmod" # ecDouble)
        # (argRoll @"r2")
        # argRoll @"pmod"
        # (argRoll @"n" # half # isZero)
