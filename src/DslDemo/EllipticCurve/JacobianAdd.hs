-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianAdd (ecDoubleJ, ecAddJ) where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.Field (feCube, feMul, feQuadruple, feSquare, feSub)
import DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    getX,
    getY,
    getZ,
    isIdentity,
    makeIdentity,
    makePoint,
  )
import Prelude hiding (drop)

ecDoubleJ :: FN (s > TPointJ) (s > TPointJ)
ecDoubleJ = function (unname @1 ecDoubleJ')

ecDoubleJ' :: FN (s > N "p" TPointJ) (s > TPointJ)
ecDoubleJ' =
  begin
    # name @"x" (pick @"p" # getX)
    # name @"y" (pick @"p" # getY)
    # name @"z" (roll @"p" # getZ)
    # name @"s" (int 4 # pick @"x" # feMul # pick @"y" # feSquare # feMul)
    # name @"m" (int 3 # roll @"x" # feSquare # feMul)
    # name @"x'" (pick @"m" # feSquare # pick @"s" # int 2 # feMul # feSub)
    # name @"y'"
      ( begin
          # (roll @"m" # roll @"s" # pick @"x'" # feSub # feMul)
          # ex1 (int 8 # pick @"y" # feQuadruple # feMul)
          # feSub
      )
    # name @"z'" (int 2 # roll @"y" # roll @"z" # feMul # feMul)
    # (roll @"x'" # roll @"y'" # roll @"z'" # makePoint)

ecAddJ :: FN (s > TPointJ > TPointJ) (s > TPointJ)
ecAddJ = function (unname @2 ecAddJ')

ecAddJ' :: FN (s > N "p1" TPointJ > N "p2" TPointJ) (s > TPointJ)
ecAddJ' =
  begin
    # (pick @"p1" # isIdentity)
    # opIf
      (roll @"p2" # drop @"p1")
      ( (pick @"p2" # isIdentity)
          # opIf
            (roll @"p1" # drop @"p2")
            doAdd
      )

doAdd :: FN (s > N "p1" TPointJ > N "p2" TPointJ) (s > TPointJ)
doAdd =
  begin
    # name @"x1" (pick @"p1" # getX)
    # name @"y1" (pick @"p1" # getY)
    # name @"z1" (pick @"p1" # getZ)
    # name @"x2" (pick @"p2" # getX)
    # name @"y2" (pick @"p2" # getY)
    # name @"z2" (roll @"p2" # getZ)
    # name @"u1" (roll @"x1" # (pick @"z2" # feSquare) # feMul)
    # name @"u2" (roll @"x2" # (pick @"z1" # feSquare) # feMul)
    # name @"s1" (roll @"y1" # (pick @"z2" # feCube) # feMul)
    # name @"s2" (roll @"y2" # (pick @"z1" # feCube) # feMul)
    # ex1 (pick @"u1" # pick @"u2" # opNumEqual)
    # opIf
      ( begin
          # drop @"z1"
          # drop @"z2"
          # drop @"u1"
          # drop @"u2"
          # (roll @"s1" # roll @"s2" # opNumNotEqual)
          # opIf
            (drop @"p1" # makeIdentity)
            (roll @"p1" # ecDoubleJ)
      )
      ( begin
          # name @"h" (roll @"u2" # pick @"u1" # feSub)
          # name @"r" (roll @"s2" # pick @"s1" # feSub)
          # name @"x3"
            ( begin
                # ex1 (pick @"r" # feSquare)
                # ex1 (pick @"h" # feCube)
                # feSub
                # ex1
                  ( begin
                      # int 2
                      # ex1
                        ( begin
                            # pick @"u1"
                            # ex1 (pick @"h" # feSquare)
                            # feMul
                        )
                      # feMul
                  )
                # feSub
            )
          # name @"y3"
            ( begin
                # roll @"r"
                # ( begin
                      # roll @"u1"
                      # pick @"h"
                      # feSquare
                      # feMul
                      # pick @"x3"
                      # feSub
                  )
                # feMul
                # (roll @"s1" # pick @"h" # feCube # feMul)
                # feSub
            )
          # name @"z3"
            (roll @"h" # roll @"z1" # feMul # roll @"z2" # feMul)
          # drop @"p1"
          # (roll @"x3" # roll @"y3" # roll @"z3" # makePoint)
      )
