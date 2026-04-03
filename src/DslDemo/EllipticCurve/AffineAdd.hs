-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.AffineAdd (ecDouble, ecAdd) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import DslDemo.EllipticCurve.Field (feAdd, feInv, feMul, feSquare, feSub)
import DslDemo.EllipticCurve.Point
  ( TPoint,
    getX,
    getY,
    isIdentity,
    makeIdentity,
    makePoint,
  )

ecDouble :: Fn (s > TPoint) (s > TPoint)
ecDouble = fn (unname 1 ecDouble')

ecDouble' :: Fn (s > N "p" TPoint) (s > TPoint)
ecDouble' =
  begin
    # name "px" (pick "p" # getX)
    # name "py" (roll "p" # getY)
    # name
      "l"
      ( begin
          # ex1 (int 3 # pick "px" # feSquare # feMul)
          # ex1 (int 2 # pick "py" # feMul # feInv)
          # feMul
      )
    # name
      "rx"
      ( begin
          # ex1 (pick "l" # feSquare)
          # ex1 (pick "px" # opDup # feAdd)
          # feSub
      )
    # name
      "ry"
      ( begin
          # (roll "l")
          # (roll "px" # pick "rx" # feSub)
          # feMul
          # roll "py"
          # feSub
      )
    # roll "rx"
    # roll "ry"
    # makePoint

ecAdd :: Fn (s > TPoint > TPoint) (s > TPoint)
ecAdd = fn (unname 2 ecAdd')

ecAdd' :: Fn (s > N "p" TPoint > N "q" TPoint) (s > TPoint)
ecAdd' =
  begin
    # (pick "p" # isIdentity)
    # opIf
      (roll "q" # del "p")
      ( (pick "q" # isIdentity)
          # opIf
            (roll "p" # del "q")
            ( pointsAreEqual
                # opIf
                  (roll "p" # ecDouble # del "q")
                  ( xCoordsEqual
                      # opIf
                        (makeIdentity # del "q" # del "p")
                        doAdd
                  )
            )
      )
  where
    pointsAreEqual = pick "p" # pick "q" # equal

    xCoordsEqual = pick "p" # getX # pick "q" # getX # opNumEqual

    doAdd :: Fn (s > N "p" TPoint > N "q" TPoint) (s > TPoint)
    doAdd =
      begin
        # name "px" (pick "p" # getX)
        # name "py" (roll "p" # getY)
        # name "qx" (pick "q" # getX)
        # name "qy" (roll "q" # getY)
        # name "xdiff" (pick "px" # pick "qx" # feSub)
        # name "ydiff" (pick "py" # roll "qy" # feSub)
        # name "l" (roll "ydiff" # roll "xdiff" # feInv # feMul)
        # name
          "rx"
          ( begin
              # (pick "l" # feSquare)
              # (pick "px" # roll "qx" # feAdd)
              # feSub
          )
        # name
          "ry"
          ( begin
              # (roll "l")
              # (roll "px" # pick "rx" # feSub)
              # feMul
              # roll "py"
              # feSub
          )
        # (roll "rx" # roll "ry" # makePoint)
