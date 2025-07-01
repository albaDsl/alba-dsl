-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.EllipticCurveUnpackedCommon
  ( setup,
    ecDouble,
    ecAdd,
  )
where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField
  ( TPrimeModulus,
    feAdd',
    feInv',
    feMul',
    feSquare',
    feSub',
  )
import DslDemo.EllipticCurve.EllipticCurvePoint
  ( TPointUnpacked,
    TPointUnpackedN,
    TTwoPointsUnpacked,
    isIdentityTag,
    makeIdentityUnpacked,
  )

setup :: FNC
setup =
  begin
    # function "ecDouble" (unname @2 ecDouble')
    # function "ecAdd" (unname @3 ecAdd')

ecDouble ::
  FN
    (Append s TPointUnpacked > TPrimeModulus)
    (Append s TPointUnpacked)
ecDouble = invoke "ecDouble" (unname @4 ecDouble')

ecDouble' ::
  FN
    (Append s (TPointUnpackedN "p") > N "pmod" TPrimeModulus)
    (Append s TPointUnpacked)
ecDouble' =
  begin
    # name @"l"
      ( begin
          # ( begin
                # int 3
                # argPick @"px"
                # argPick @"pmod"
                # feSquare'
                # argPick @"pmod"
                # feMul'
            )
          # ( begin
                # int 2
                # argPick @"py"
                # argPick @"pmod"
                # feMul'
                # argPick @"pmod"
                # feInv'
            )
          # argPick @"pmod"
          # feMul'
      )
    # name @"rx"
      ( begin
          # (argPick @"l" # argPick @"pmod" # feSquare')
          # (argPick @"px" # opDup # argPick @"pmod" # feAdd')
          # argPick @"pmod"
          # feSub'
      )
    # name @"ry"
      ( begin
          # argRoll @"l"
          # (argRoll @"px" # argPick @"rx" # argPick @"pmod" # feSub')
          # argPick @"pmod"
          # feMul'
          # argRoll @"py"
          # argRoll @"pmod"
          # feSub'
      )
    # argRoll @"ptag"
    # argRoll @"rx"
    # argRoll @"ry"

ecAdd ::
  FN
    (Append s TTwoPointsUnpacked > TPrimeModulus)
    (Append s TPointUnpacked)
ecAdd = invoke "ecAdd" (unname @7 ecAdd')

ecAdd' ::
  FN
    ( Append s (Append (TPointUnpackedN "p") (TPointUnpackedN "q"))
        > N "pmod" TPrimeModulus
    )
    (Append s TPointUnpacked)
ecAdd' =
  begin
    # (argPick @"ptag" # isIdentityTag)
    # opIf
      (argRoll @"qtag" # argRoll @"qx" # argRoll @"qy" # argsDrop @4)
      ( (argPick @"qtag" # isIdentityTag)
          # opIf
            (argRoll @"ptag" # argRoll @"px" # argRoll @"py" # argsDrop @4)
            ( nonIdentityPointsAreEqual
                # opIf
                  ( begin
                      # argRoll @"qtag"
                      # argRoll @"qx"
                      # argRoll @"qy"
                      # argRoll @"pmod"
                      # ecDouble
                      # argsDrop @3
                  )
                  ( xCoordsEqual
                      # opIf
                        (makeIdentityUnpacked # argsDrop @7)
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
        ( Append s (Append (TPointUnpackedN "p") (TPointUnpackedN "q"))
            > N "pmod" TPrimeModulus
        )
        (Append s TPointUnpacked)
    doAdd =
      begin
        # name @"xdiff"
          ( begin
              # argPick @"px"
              # argPick @"qx"
              # argPick @"pmod"
              # feSub'
          )
        # name @"ydiff"
          ( begin
              # argPick @"py"
              # argRoll @"qy"
              # argPick @"pmod"
              # feSub'
          )
        # name @"l"
          ( begin
              # argRoll @"ydiff"
              # argRoll @"xdiff"
              # argPick @"pmod"
              # feInv'
              # argPick @"pmod"
              # feMul'
          )
        # name @"rx"
          ( begin
              # (argPick @"l" # argPick @"pmod" # feSquare')
              # (argPick @"px" # argRoll @"qx" # argPick @"pmod" # feAdd')
              # argPick @"pmod"
              # feSub'
          )
        # name @"ry"
          ( begin
              # (argRoll @"l")
              # (argRoll @"px" # argPick @"rx" # argPick @"pmod" # feSub')
              # argPick @"pmod"
              # feMul'
              # argRoll @"py"
              # argRoll @"pmod"
              # feSub'
          )
        # (argRoll @"ptag" # argRoll @"rx" # argRoll @"ry")
        # argDrop @"qtag"
