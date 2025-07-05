-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.AffineAdd (ecDouble, ecAdd) where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.Field (feAdd, feInv, feMul, feSquare, feSub)
import DslDemo.EllipticCurve.Point
  ( TPoint,
    getX,
    getY,
    isEqual,
    isIdentity,
    makeIdentity,
    makePoint,
  )

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = function (unname @1 ecDouble')

ecDouble' :: FN (s > N "p" TPoint) (s > TPoint)
ecDouble' =
  begin
    # name @"px" (argPick @"p" # getX)
    # name @"py" (argRoll @"p" # getY)
    # name @"l"
      ( begin
          # ex1 (int 3 # argPick @"px" # feSquare # feMul)
          # ex1 (int 2 # argPick @"py" # feMul # feInv)
          # feMul
      )
    # name @"rx"
      ( begin
          # ex1 (argPick @"l" # feSquare)
          # ex1 (argPick @"px" # opDup # feAdd)
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
    # argRoll @"rx"
    # argRoll @"ry"
    # makePoint

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd = function (unname @2 ecAdd')

ecAdd' :: FN (s > N "p" TPoint > N "q" TPoint) (s > TPoint)
ecAdd' =
  begin
    # (argPick @"p" # isIdentity)
    # opIf
      (argRoll @"q" # argsDrop @1)
      ( (argPick @"q" # isIdentity)
          # opIf
            (argRoll @"p" # argDrop @"q")
            ( pointsAreEqual
                # opIf
                  (argRoll @"p" # ecDouble # argDrop @"q")
                  ( xCoordsEqual
                      # opIf
                        (makeIdentity # argsDrop @2)
                        doAdd
                  )
            )
      )
  where
    pointsAreEqual = argPick @"p" # argPick @"q" # isEqual

    xCoordsEqual = argPick @"p" # getX # argPick @"q" # getX # opNumEqual

    doAdd :: FN (s > N "p" TPoint > N "q" TPoint) (s > TPoint)
    doAdd =
      begin
        # name @"px" (argPick @"p" # getX)
        # name @"py" (argRoll @"p" # getY)
        # name @"qx" (argPick @"q" # getX)
        # name @"qy" (argRoll @"q" # getY)
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
        # (argRoll @"rx" # argRoll @"ry" # makePoint)
