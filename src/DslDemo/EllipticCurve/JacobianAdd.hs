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

ecDoubleJ :: FN (s > TPointJ) (s > TPointJ)
ecDoubleJ = function (unname @1 ecDoubleJ')

ecDoubleJ' :: FN (s > N "p" TPointJ) (s > TPointJ)
ecDoubleJ' =
  begin
    # name @"x" (argPick @"p" # getX)
    # name @"y" (argPick @"p" # getY)
    # name @"z" (argRoll @"p" # getZ)
    # name @"s"
      ( begin
          # ex1 (int 4 # argPick @"x" # feMul)
          # ex1 (argPick @"y" # feSquare)
          # feMul
      )
    # name @"m" (int 3 # (argRoll @"x" # feSquare) # feMul)
    # name @"x'"
      ( begin
          # ex1 (argPick @"m" # feSquare)
          # ex1 (argPick @"s" # int 2 # feMul)
          # feSub
      )
    # name @"y'"
      ( begin
          # argRoll @"m"
          # (argRoll @"s" # argPick @"x'" # feSub)
          # feMul
          # ex1
            ( begin
                # int 8
                # argPick @"y"
                # feQuadruple
                # feMul
            )
          # feSub
      )
    # name @"z'"
      ( begin
          # int 2
          # (argRoll @"y" # argRoll @"z" # feMul)
          # feMul
      )
    # argRoll @"x'"
    # argRoll @"y'"
    # argRoll @"z'"
    # makePoint

ecAddJ :: FN (s > TPointJ > TPointJ) (s > TPointJ)
ecAddJ = function (unname @2 ecAddJ')

ecAddJ' :: FN (s > N "p1" TPointJ > N "p2" TPointJ) (s > TPointJ)
ecAddJ' =
  begin
    # (argPick @"p1" # isIdentity)
    # opIf
      (argRoll @"p2" # argDrop @"p1")
      ( (argPick @"p2" # isIdentity)
          # opIf
            (argRoll @"p1" # argDrop @"p2")
            doAdd
      )

doAdd :: FN (s > N "p1" TPointJ > N "p2" TPointJ) (s > TPointJ)
doAdd =
  begin
    # name @"x1" (argPick @"p1" # getX)
    # name @"y1" (argPick @"p1" # getY)
    # name @"z1" (argPick @"p1" # getZ)
    # name @"x2" (argPick @"p2" # getX)
    # name @"y2" (argPick @"p2" # getY)
    # name @"z2" (argRoll @"p2" # getZ)
    # name @"u1" (argRoll @"x1" # (argPick @"z2" # feSquare) # feMul)
    # name @"u2" (argRoll @"x2" # (argPick @"z1" # feSquare) # feMul)
    # name @"s1" (argRoll @"y1" # (argPick @"z2" # feCube) # feMul)
    # name @"s2" (argRoll @"y2" # (argPick @"z1" # feCube) # feMul)
    # ex1 (argPick @"u1" # argPick @"u2" # opNumEqual)
    # opIf
      ( begin
          # argDrop @"z1"
          # argDrop @"z2"
          # argDrop @"u1"
          # argDrop @"u2"
          # (argRoll @"s1" # argRoll @"s2" # opNumNotEqual)
          # opIf
            (argDrop @"p1" # makeIdentity)
            (argRoll @"p1" # ecDoubleJ)
      )
      ( begin
          # name @"h" (argRoll @"u2" # argPick @"u1" # feSub)
          # name @"r" (argRoll @"s2" # argPick @"s1" # feSub)
          # name @"x3"
            ( begin
                # ex1 (argPick @"r" # feSquare)
                # ex1 (argPick @"h" # feCube)
                # feSub
                # ex1
                  ( begin
                      # int 2
                      # ex1
                        ( begin
                            # argPick @"u1"
                            # ex1 (argPick @"h" # feSquare)
                            # feMul
                        )
                      # feMul
                  )
                # feSub
            )
          # name @"y3"
            ( begin
                # argRoll @"r"
                # ( begin
                      # argRoll @"u1"
                      # argPick @"h"
                      # feSquare
                      # feMul
                      # argPick @"x3"
                      # feSub
                  )
                # feMul
                # (argRoll @"s1" # argPick @"h" # feCube # feMul)
                # feSub
            )
          # name @"z3"
            (argRoll @"h" # argRoll @"z1" # feMul # argRoll @"z2" # feMul)
          # argDrop @"p1"
          # (argRoll @"x3" # argRoll @"y3" # argRoll @"z3" # makePoint)
      )
