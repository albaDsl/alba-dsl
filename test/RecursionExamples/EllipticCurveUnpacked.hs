{-# OPTIONS_GHC -w #-}

module RecursionExamples.EllipticCurveUnpacked (ecDouble, ecAdd, ecMul) where

import Alba.Dsl.V1.Bch2025.Contract.Math (isEven)
import Alba.Dsl.V1.Bch2026
import RecursionExamples.EllipticCurveField
  ( feAdd,
    feInv,
    feMul,
    feSquare,
    feSub,
  )
import RecursionExamples.EllipticCurvePoint
  ( TPointUnpacked,
    TPointUnpackedN,
    TTwoPointsUnpacked,
    isIdentityTag,
    makeIdentityUnpacked,
  )

ecDouble :: FN (Append s TPointUnpacked) (Append s TPointUnpacked)
ecDouble = unname @3 ecDouble'

ecDouble' :: FN (Append s (TPointUnpackedN "p")) (Append s TPointUnpacked)
ecDouble' =
  begin
    # name @"l"
      ( begin
          # (int 3 # argPick @"px" # feSquare # feMul)
          # (int 2 # argPick @"py" # feMul # feInv)
          # feMul
      )
    # name @"rx"
      ( begin
          # (argPick @"l" # feSquare)
          # (argPick @"px" # opDup # feAdd)
          # feSub
      )
    # name @"ry"
      ( begin
          # argRoll @"l"
          # (argRoll @"px" # argPick @"rx" # feSub)
          # feMul
          # argRoll @"py"
          # feSub
      )
    # argRoll @"ptag"
    # argRoll @"rx"
    # argRoll @"ry"

ecAdd :: FN (Append s TTwoPointsUnpacked) (Append s TPointUnpacked)
ecAdd = unname @6 ecAdd'

ecAdd' ::
  FN
    (Append s (Append (TPointUnpackedN "p") (TPointUnpackedN "q")))
    (Append s TPointUnpacked)
ecAdd' =
  begin
    # (argPick @"ptag" # isIdentityTag)
    # opIf
      (argRoll @"qtag" # argRoll @"qx" # argRoll @"qy" # argsDrop @3)
      ( (argPick @"qtag" # isIdentityTag)
          # opIf
            (argRoll @"ptag" # argRoll @"px" # argRoll @"py" # argsDrop @3)
            ( nonIdentityPointsAreEqual
                # opIf
                  ( begin
                      # argRoll @"qtag"
                      # argRoll @"qx"
                      # argRoll @"qy"
                      # ecDouble
                      # argsDrop @3
                  )
                  ( xCoordsEqual
                      # opIf
                        (makeIdentityUnpacked # argsDrop @6)
                        doAdd
                  )
            )
      )
  where
    nonIdentityPointsAreEqual =
      begin
        # (argPick @"px" # argPick @"qx" # opNumEqual)
        # (argPick @"py" # argPick @"qy" # opNumEqual)
        # opBoolAnd

    xCoordsEqual = argPick @"px" # argPick @"qx" # opNumEqual

    doAdd ::
      FN
        (Append s (Append (TPointUnpackedN "p") (TPointUnpackedN "q")))
        (Append s TPointUnpacked)
    doAdd =
      begin
        # name @"xdiff" (argPick @"px" # argPick @"qx" # feSub)
        # name @"ydiff" (argPick @"py" # argRoll @"qy" # feSub)
        # name @"l" (argRoll @"ydiff" # argRoll @"xdiff" # feInv # feMul)
        # name @"rx"
          ( begin
              # (argPick @"l" # feSquare)
              # (argPick @"px" # argRoll @"qx" # feAdd)
              # feSub
          )
        # name @"ry"
          ( begin
              # (argRoll @"l")
              # (argRoll @"px" # argPick @"rx" # feSub)
              # feMul
              # argRoll @"py"
              # feSub
          )
        # (argRoll @"ptag" # argRoll @"rx" # argRoll @"ry")
        # argDrop @"qtag"

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
              # argPick @"n"
              # isEven
              # opIf
                ( begin
                    # (argRoll @"n" # nat 2 # opDiv)
                    # ( begin
                          # argRoll @"ptag"
                          # argRoll @"px"
                          # argRoll @"py"
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
                    # ecAdd
                )
          )
