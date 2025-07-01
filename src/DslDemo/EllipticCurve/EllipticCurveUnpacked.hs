-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.EllipticCurveUnpacked
  ( setup,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Math (half, isOdd)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField (TPrimeModulus, primeModulus)
import DslDemo.EllipticCurve.EllipticCurvePoint
  ( TPoint,
    TPointUnpacked,
    TPointUnpackedN,
    makeIdentityUnpacked,
    namePoint,
    packPoint,
    unpackPoint,
  )
import DslDemo.EllipticCurve.EllipticCurveUnpackedCommon qualified as EU

type LoopTypeN s =
  ( Append
      (Append (s > N "n" TNat) (TPointUnpackedN "p"))
      (TPointUnpackedN "r")
      > N "pmod" TPrimeModulus
  )

type LoopType s =
  (Append (Append (s > TNat) TPointUnpacked) TPointUnpacked > TPrimeModulus)

setup :: FNC
setup =
  begin
    # EU.setup
    # function "ecMulInternal" ecMulInternal'

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd =
  begin
    # unpackPoint
    # opRoll @3
    # unpackPoint
    # primeModulus
    # EU.ecAdd
    # packPoint

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = unpackPoint # primeModulus # EU.ecDouble # packPoint

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = unpackPoint # ecMulInternal # packPoint

ecMulInternal :: FN (Append (s > TNat) TPointUnpacked) (Append s TPointUnpacked)
ecMulInternal = invoke "ecMulInternal" (unname @4 ecMulInternal')

ecMulInternal' ::
  FN
    (Append (s > N "n" TNat) (TPointUnpackedN "p"))
    (Append s TPointUnpacked)
ecMulInternal' =
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
  where
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
                    # EU.ecAdd
                )
          )
        # (argPick @"n" # half)
        # ( begin
              # argRoll @"ptag"
              # argRoll @"px"
              # argRoll @"py"
              # argPick @"pmod"
              # EU.ecDouble
          )
        # (argRoll @"r2tag" # argRoll @"r2x" # argRoll @"r2y")
        # argRoll @"pmod"
        # (argRoll @"n" # half # isZero)
