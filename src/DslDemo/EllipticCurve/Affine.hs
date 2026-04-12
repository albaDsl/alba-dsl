-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Affine (ecDouble, ecAdd, ecMul) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    TNat,
    begin,
    del,
    ex1,
    fn,
    name,
    nat,
    ns2,
    ns3,
    opIf,
    opNip,
    opNumEqual,
    opUntil,
    opWhen,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude (halve, isOdd, isZero)
import DslDemo.EllipticCurve.AffineAdd (ecAdd, ecDouble)
import DslDemo.EllipticCurve.Point (TPoint, makeIdentity)

ecMul :: Fn (s > TNat > TPoint) (s > TPoint)
ecMul =
  fn
    ( begin
        # (ns2 "n" "p" # pick "n" # nat 0 # opNumEqual)
        # opIf
          (del "n" # del "p" # makeIdentity)
          (roll "n" # roll "p" # makeIdentity # opUntil loop # opNip # opNip)
    )
  where
    loop :: Loop (s > TNat > TPoint > TPoint)
    loop =
      begin
        # ns3 "n" "p" "r"
        # name
          "r2"
          (roll "r" # ex1 (pick "n" # isOdd) # opWhen (pick "p" # ecAdd))
        # (pick "n" # halve # roll "p" # ecDouble # roll "r2")
        # (roll "n" # halve # isZero)
