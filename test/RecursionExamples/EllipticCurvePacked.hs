{-# OPTIONS_GHC -w #-}

module RecursionExamples.EllipticCurvePacked (ecDouble, ecAdd, ecMul) where

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
  ( TPoint,
    getX,
    getY,
    isEqual,
    isIdentity,
    makeIdentity,
    makePoint,
  )

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = unname @1 ecDouble'
  where
    ecDouble' :: FN (s > N "p" TPoint) (s > TPoint)
    ecDouble' =
      begin
        # name @"px" (argPick @"p" # getX)
        # name @"py" (argPick @"p" # getY)
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
              # (argPick @"l")
              # (argPick @"px" # argPick @"rx" # feSub)
              # feMul
              # argPick @"py"
              # feSub
          )
        # argPick @"rx"
        # argPick @"ry"
        # makePoint
        # argsDrop @6

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd = unname @2 ecAdd'

ecAdd' :: FN (s > N "p" TPoint > N "q" TPoint) (s > TPoint)
ecAdd' =
  begin
    # (argPick @"p" # isIdentity)
    # opIf
      (argRoll @"q" # argsDrop @1)
      ( (argPick @"q" # isIdentity)
          # opIf
            (argRoll @"p" # argsDrop @1)
            ( pointsAreEqual
                # opIf
                  (argRoll @"p" # ecDouble # argsDrop @1)
                  ( xCoordsEqual
                      # opIf
                        (makeIdentity # argsDrop @2)
                        ( begin
                            # argRoll @"p"
                            # argRoll @"q"
                            # unname @2 doAdd
                        )
                  )
            )
      )
  where
    pointsAreEqual = argPick @"p" # argPick @"q" # isEqual

    xCoordsEqual = (argPick @"p" # getX) # (argPick @"q" # getX) # opNumEqual

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
                    # (argPick @"p" # ecDouble)
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
                    # ecAdd
                    # argsDrop @3
                )
          )
