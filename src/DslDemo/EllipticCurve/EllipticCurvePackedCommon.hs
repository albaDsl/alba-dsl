module DslDemo.EllipticCurve.EllipticCurvePackedCommon
  ( ecDouble,
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
  ( TPoint,
    getX,
    getY,
    isEqual,
    isIdentity,
    makeIdentity,
    makePoint,
  )

ecDouble :: FN (s > TPoint > TPrimeModulus) (s > TPoint)
ecDouble = unname @2 ecDouble'
  where
    ecDouble' :: FN (s > N "p" TPoint > N "pmod" TPrimeModulus) (s > TPoint)
    ecDouble' =
      begin
        # name @"px" (argPick @"p" # getX)
        # name @"py" (argRoll @"p" # getY)
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
              # (argRoll @"l")
              # (argRoll @"px" # argPick @"rx" # argPick @"pmod" # feSub')
              # argPick @"pmod"
              # feMul'
              # argRoll @"py"
              # argRoll @"pmod"
              # feSub'
          )
        # argRoll @"rx"
        # argRoll @"ry"
        # makePoint

ecAdd :: FN (s > TPoint > TPoint > TPrimeModulus) (s > TPoint)
ecAdd = unname @3 ecAdd'

ecAdd' ::
  FN
    (s > N "p" TPoint > N "q" TPoint > N "pmod" TPrimeModulus)
    (s > TPoint)
ecAdd' =
  begin
    # (argPick @"p" # isIdentity)
    # opIf
      (argRoll @"q" # argsDrop @2)
      ( (argPick @"q" # isIdentity)
          # opIf
            (argRoll @"p" # argsDrop @2)
            ( pointsAreEqual
                # opIf
                  (argRoll @"p" # argRoll @"pmod" # ecDouble # argsDrop @1)
                  ( xCoordsEqual
                      # opIf
                        (makeIdentity # argsDrop @3)
                        doAdd
                  )
            )
      )
  where
    pointsAreEqual = argPick @"p" # argPick @"q" # isEqual

    xCoordsEqual = (argPick @"p" # getX) # (argPick @"q" # getX) # opNumEqual

    doAdd ::
      FN
        (s > N "p" TPoint > N "q" TPoint > N "pmod" TPrimeModulus)
        (s > TPoint)
    doAdd =
      begin
        # name @"px" (argPick @"p" # getX)
        # name @"py" (argRoll @"p" # getY)
        # name @"qx" (argPick @"q" # getX)
        # name @"qy" (argRoll @"q" # getY)
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
        # (argRoll @"rx" # argRoll @"ry" # makePoint)
