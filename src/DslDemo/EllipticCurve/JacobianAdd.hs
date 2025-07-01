-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianAdd
  ( setup,
    ecDouble,
    ecDouble',
    ecAdd,
    ecAdd',
  )
where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.Field
  ( TPrimeModulus,
    feCube',
    feMul',
    feQuadruple',
    feSquare',
    feSub',
  )
import DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    getX,
    getY,
    getZ,
    isIdentity,
    makeIdentity,
    makePoint,
  )

setup :: FNC
setup =
  begin
    # function "ecDouble" (unname @2 ecDouble')
    # function "ecAdd" (unname @3 ecAdd')

ecDouble :: FN (s > TPointJ > TPrimeModulus) (s > TPointJ)
ecDouble = invoke "ecDouble" (unname @2 ecDouble')

ecDouble' :: FN (s > N "p" TPointJ > N "pmod" TPrimeModulus) (s > TPointJ)
ecDouble' =
  begin
    # name @"x" (argPick @"p" # getX)
    # name @"y" (argPick @"p" # getY)
    # name @"z" (argRoll @"p" # getZ)
    # name @"s"
      ( begin
          # ex1 (int 4 # argPick @"x" # argPick @"pmod" # feMul')
          # ex1 (argPick @"y" # argPick @"pmod" # feSquare')
          # argPick @"pmod"
          # feMul'
      )
    # name @"m"
      ( begin
          # int 3
          # (argRoll @"x" # argPick @"pmod" # feSquare')
          # argPick @"pmod"
          # feMul'
      )
    # name @"x'"
      ( begin
          # ex1 (argPick @"m" # argPick @"pmod" # feSquare')
          # ex1 (argPick @"s" # int 2 # argPick @"pmod" # feMul')
          # argPick @"pmod"
          # feSub'
      )
    # name @"y'"
      ( begin
          # argRoll @"m"
          # (argRoll @"s" # argPick @"x'" # argPick @"pmod" # feSub')
          # argPick @"pmod"
          # feMul'
          # ex1
            ( begin
                # int 8
                # argPick @"y"
                # argPick @"pmod"
                # feQuadruple'
                # argPick @"pmod"
                # feMul'
            )
          # argPick @"pmod"
          # feSub'
      )
    # name @"z'"
      ( begin
          # int 2
          # (argRoll @"y" # argRoll @"z" # argPick @"pmod" # feMul')
          # argRoll @"pmod"
          # feMul'
      )
    # argRoll @"x'"
    # argRoll @"y'"
    # argRoll @"z'"
    # makePoint

ecAdd :: FN (s > TPointJ > TPointJ > TPrimeModulus) (s > TPointJ)
ecAdd = invoke "ecAdd" (unname @3 ecAdd')

ecAdd' ::
  FN
    (s > N "p1" TPointJ > N "p2" TPointJ > N "pmod" TPrimeModulus)
    (s > TPointJ)
ecAdd' =
  begin
    # (argPick @"p1" # isIdentity)
    # opIf
      (argRoll @"p2" # argsDrop @2)
      ( (argPick @"p2" # isIdentity)
          # opIf
            (argRoll @"p1" # argsDrop @2)
            doAdd
      )

doAdd ::
  FN
    (s > N "p1" TPointJ > N "p2" TPointJ > N "pmod" TPrimeModulus)
    (s > TPointJ)
doAdd =
  begin
    # name @"x1" (argPick @"p1" # getX)
    # name @"y1" (argPick @"p1" # getY)
    # name @"z1" (argPick @"p1" # getZ)
    # name @"x2" (argPick @"p2" # getX)
    # name @"y2" (argPick @"p2" # getY)
    # name @"z2" (argRoll @"p2" # getZ)
    # name @"u1"
      ( begin
          # argRoll @"x1"
          # (argPick @"z2" # argPick @"pmod" # feSquare')
          # argPick @"pmod"
          # feMul'
      )
    # name @"u2"
      ( begin
          # argRoll @"x2"
          # (argPick @"z1" # argPick @"pmod" # feSquare')
          # argPick @"pmod"
          # feMul'
      )
    # name @"s1"
      ( begin
          # argRoll @"y1"
          # (argPick @"z2" # argPick @"pmod" # feCube')
          # argPick @"pmod"
          # feMul'
      )
    # name @"s2"
      ( begin
          # argRoll @"y2"
          # (argPick @"z1" # argPick @"pmod" # feCube')
          # argPick @"pmod"
          # feMul'
      )
    # ex1 (argPick @"u1" # argPick @"u2" # opNumEqual)
    # opIf
      ( begin
          # argDrop @"z1"
          # argDrop @"z2"
          # argDrop @"u1"
          # argDrop @"u2"
          # (argRoll @"s1" # argRoll @"s2" # opNumNotEqual)
          # opIf
            (argDrop @"p1" # argDrop @"pmod" # makeIdentity)
            (argRoll @"p1" # argRoll @"pmod" # ecDouble)
      )
      ( begin
          # name @"h" (argRoll @"u2" # argPick @"u1" # argPick @"pmod" # feSub')
          # name @"r" (argRoll @"s2" # argPick @"s1" # argPick @"pmod" # feSub')
          # name @"x3"
            ( begin
                # ex1 (argPick @"r" # argPick @"pmod" # feSquare')
                # ex1 (argPick @"h" # argPick @"pmod" # feCube')
                # argPick @"pmod"
                # feSub'
                # ex1
                  ( begin
                      # int 2
                      # ex1
                        ( begin
                            # argPick @"u1"
                            # ex1 (argPick @"h" # argPick @"pmod" # feSquare')
                            # argPick @"pmod"
                            # feMul'
                        )
                      # argPick @"pmod"
                      # feMul'
                  )
                # argPick @"pmod"
                # feSub'
            )
          # name @"y3"
            ( begin
                # argRoll @"r"
                # ( begin
                      # argRoll @"u1"
                      # argPick @"h"
                      # argPick @"pmod"
                      # feSquare'
                      # argPick @"pmod"
                      # feMul'
                      # argPick @"x3"
                      # argPick @"pmod"
                      # feSub'
                  )
                # argPick @"pmod"
                # feMul'
                # ( begin
                      # argRoll @"s1"
                      # argPick @"h"
                      # argPick @"pmod"
                      # feCube'
                      # argPick @"pmod"
                      # feMul'
                  )
                # argPick @"pmod"
                # feSub'
            )
          # name @"z3"
            ( begin
                # argRoll @"h"
                # argRoll @"z1"
                # argPick @"pmod"
                # feMul'
                # argRoll @"z2"
                # argPick @"pmod"
                # feMul'
            )
          # argDrop @"p1"
          # argDrop @"pmod"
          # argRoll @"x3"
          # argRoll @"y3"
          # argRoll @"z3"
          # makePoint
      )
