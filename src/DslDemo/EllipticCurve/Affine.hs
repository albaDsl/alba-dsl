-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Affine
  ( setup,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Math (half, isOdd)
import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.AffineAdd qualified as EP
import DslDemo.EllipticCurve.Field (TPrimeModulus, primeModulus)
import DslDemo.EllipticCurve.Point (TPoint, makeIdentity)

type LoopTypeN s =
  s
    > N "n" TNat
    > N "p" TPoint
    > N "r" TPoint
    > N "pmod" TPrimeModulus

type LoopType s = s > TNat > TPoint > TPoint > TPrimeModulus

setup :: FNC
setup =
  begin
    # EP.setup
    # function "ecMul" ecMul'

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd = primeModulus # EP.ecAdd

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = primeModulus # EP.ecDouble

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = invoke "ecMul" (unname @2 ecMul')

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
          # primeModulus
          # opUntil (unname @4 loop)
          # opDrop
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
              # opWhen (argPick @"p" # argPick @"pmod" # EP.ecAdd)
          )
        # (argPick @"n" # half)
        # (argRoll @"p" # argPick @"pmod" # EP.ecDouble)
        # (argRoll @"r2")
        # argRoll @"pmod"
        # (argRoll @"n" # half # isZero)
