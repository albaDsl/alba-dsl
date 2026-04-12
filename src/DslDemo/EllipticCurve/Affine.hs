-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Affine (ecDouble, ecAdd, ecMul) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude (halve, isOdd, isZero)
import DslDemo.EllipticCurve.AffineAdd (ecAdd, ecDouble)
import DslDemo.EllipticCurve.Point (TPoint, makeIdentity)

type LoopTypeN s = s > N "n" TNat > N "p" TPoint > N "r" TPoint

type LoopType s = s > TNat > TPoint > TPoint

ecMul :: Fn (s > TNat > TPoint) (s > TPoint)
ecMul = fn (unname 2 ecMul')

ecMul' :: Fn (s > N "n" TNat > N "p" TPoint) (s > TPoint)
ecMul' =
  begin
    # pick "n"
    # (nat 0 # opNumEqual)
    # opIf
      (del "n" # del "p" # makeIdentity)
      ( begin
          # roll "n"
          # roll "p"
          # makeIdentity
          # opUntil (unname 3 loop)
          # opNip
          # opNip
      )
  where
    loop :: Fn (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # name
          "r2"
          ( begin
              # roll "r"
              # ex1 (pick "n" # isOdd)
              # opWhen (pick "p" # ecAdd)
          )
        # (pick "n" # halve)
        # (roll "p" # ecDouble)
        # (roll "r2")
        # (roll "n" # halve # isZero)
