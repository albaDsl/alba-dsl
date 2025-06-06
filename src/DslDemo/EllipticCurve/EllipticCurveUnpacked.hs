module DslDemo.EllipticCurve.EllipticCurveUnpacked
  ( ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Math (isEven)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField (primeModulus)
import DslDemo.EllipticCurve.EllipticCurvePoint
  ( TPointUnpacked,
    TPointUnpackedN,
  )
import DslDemo.EllipticCurve.EllipticCurveUnpackedCommon (ecAdd, ecDouble)

ecMul :: FN (Append (s > TNat) TPointUnpacked) (Append s TPointUnpacked)
ecMul = lambda' mul # recur mul
  where
    mul ::
      FN
        (Append (s > TNat) TPointUnpacked > TLambdaUntyped)
        (Append s TPointUnpacked)
    mul = unname @5 mul'

    mul' ::
      FN
        (Append (s > N "n" TNat) (TPointUnpackedN "p") > N "mul" TLambdaUntyped)
        (Append s TPointUnpacked)
    mul' =
      begin
        # argPick @"n"
        # (nat 1 # opNumEqual)
        # opIf
          (argRoll @"ptag" # argRoll @"px" # argRoll @"py" # argsDrop @2)
          ( begin
              # name @"pmod" primeModulus
              # argPick @"n"
              # isEven
              # opIf
                ( begin
                    # (argRoll @"n" # nat 2 # opDiv)
                    # ( begin
                          # argRoll @"ptag"
                          # argRoll @"px"
                          # argRoll @"py"
                          # argRoll @"pmod"
                          # ecDouble
                      )
                    # argRoll @"mul"
                    # recur mul
                )
                ( begin
                    # (argPick @"ptag" # argPick @"px" # argPick @"py")
                    # ( begin
                          # (argRoll @"n" # nat 1 # opSubUnsafe)
                          # argRoll @"ptag"
                          # argRoll @"px"
                          # argRoll @"py"
                          # argRoll @"mul"
                          # recur mul
                      )
                    # argRoll @"pmod"
                    # ecAdd
                )
          )
