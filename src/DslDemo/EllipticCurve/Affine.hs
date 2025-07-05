-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Affine (ecDouble, ecAdd, ecMul) where

import Alba.Dsl.V1.Bch2025.Contract.Math (half, isOdd)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.AffineAdd (ecAdd, ecDouble)
import DslDemo.EllipticCurve.Point (TPoint, makeIdentity)

type LoopTypeN s = s > N "n" TNat > N "p" TPoint > N "r" TPoint

type LoopType s = s > TNat > TPoint > TPoint

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = function (unname @2 ecMul')

ecMul' :: FN (s > N "n" TNat > N "p" TPoint) (s > TPoint)
ecMul' =
  begin
    # argPick @"n"
    # (nat 0 # opNumEqual)
    # opIf
      (argDrop @"n" # argDrop @"p" # makeIdentity)
      ( begin
          # argRoll @"n"
          # argRoll @"p"
          # makeIdentity
          # opUntil (unname @3 loop)
          # opNip
          # opNip
      )
  where
    loop :: FN (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # name @"r2"
          ( begin
              # argRoll @"r"
              # ex1 (argPick @"n" # isOdd)
              # opWhen (argPick @"p" # ecAdd)
          )
        # (argPick @"n" # half)
        # (argRoll @"p" # ecDouble)
        # (argRoll @"r2")
        # (argRoll @"n" # half # isZero)
