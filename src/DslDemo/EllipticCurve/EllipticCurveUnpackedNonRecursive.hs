module DslDemo.EllipticCurve.EllipticCurveUnpackedNonRecursive
  ( ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Math (half, isOdd)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField (TPrimeModulus, primeModulus)
import DslDemo.EllipticCurve.EllipticCurvePoint
  ( TPointUnpacked,
    TPointUnpackedN,
    makeIdentityUnpacked,
    namePoint,
  )
import DslDemo.EllipticCurve.EllipticCurveUnpackedCommon (ecAdd, ecDouble)

type LoopTypeN s =
  ( Append
      (Append (s > N "n" TNat) (TPointUnpackedN "p"))
      (TPointUnpackedN "r")
      > N "pmod" TPrimeModulus
  )

type LoopType s =
  (Append (Append (s > TNat) TPointUnpacked) TPointUnpacked > TPrimeModulus)

ecMul :: FN (Append (s > TNat) TPointUnpacked) (Append s TPointUnpacked)
ecMul = unname @4 ecMul'
  where
    ecMul' ::
      FN
        (Append (s > N "n" TNat) (TPointUnpackedN "p"))
        (Append s TPointUnpacked)
    ecMul' =
      begin
        # argPick @"n"
        # (nat 1 # opNumEqual)
        # opIf
          (argRoll @"ptag" # argRoll @"px" # argRoll @"py" # argsDrop @1)
          ( begin
              # argRoll @"n"
              # (argRoll @"ptag" # argRoll @"px" # argRoll @"py")
              # makeIdentityUnpacked
              # primeModulus
              # opUntil (unname @8 loop)
              # opDrop
              # (opToAltStack # opToAltStack # opToAltStack)
              # (op2Drop # op2Drop)
              # (opFromAltStack # opFromAltStack # opFromAltStack)
          )

    loop :: FN (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # namePoint @"r2"
          ( begin
              # (argRoll @"rtag" # argRoll @"rx" # argRoll @"ry")
              # ex1 (argPick @"n" # isOdd)
              # opWhen
                ( begin
                    # (argPick @"ptag" # argPick @"px" # argPick @"py")
                    # argPick @"pmod"
                    # ecAdd
                )
          )
        # (argPick @"n" # half)
        # ( begin
              # argRoll @"ptag"
              # argRoll @"px"
              # argRoll @"py"
              # argPick @"pmod"
              # ecDouble
          )
        # (argRoll @"r2tag" # argRoll @"r2x" # argRoll @"r2y")
        # argRoll @"pmod"
        # (argRoll @"n" # half # isZero)
