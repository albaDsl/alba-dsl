module DslDemo.EllipticCurve.EllipticCurvePacked (ecDouble, ecAdd, ecMul) where

import Alba.Dsl.V1.Bch2025.Contract.Math (isEven)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField (primeModulus)
import DslDemo.EllipticCurve.EllipticCurvePackedCommon (ecAdd, ecDouble)
import DslDemo.EllipticCurve.EllipticCurvePoint (TPoint)

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = lambda' mul # recur mul
  where
    mul :: FN (s > TNat > TPoint > TLambdaUntyped) (s > TPoint)
    mul = unname @3 mul'

    mul' ::
      FN
        (s > N "n" TNat > N "p" TPoint > N "mul" TLambdaUntyped)
        (s > TPoint)
    mul' =
      begin
        # argPick @"n"
        # (nat 1 # opNumEqual)
        # opIf
          (argPick @"p" # argsDrop @3)
          ( begin
              # argPick @"n"
              # isEven
              # opIf
                ( begin
                    # (argPick @"n" # nat 2 # opDiv)
                    # (argPick @"p" # primeModulus # ecDouble)
                    # argPick @"mul"
                    # recur mul
                    # argsDrop @3
                )
                ( begin
                    # argPick @"p"
                    # ( begin
                          # (argPick @"n" # nat 1 # opSubUnsafe)
                          # argPick @"p"
                          # argPick @"mul"
                          # recur mul
                      )
                    # primeModulus
                    # ecAdd
                    # argsDrop @3
                )
          )
