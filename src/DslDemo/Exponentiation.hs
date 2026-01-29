-- Copyright (c) 2025 albaDsl

module DslDemo.Exponentiation (pow) where

import Alba.Dsl.V1.Bch2025.Contract.Math (isEven)
import Alba.Dsl.V1.Bch2026

pow :: FN (s > TInt > TNat) (s > TInt)
pow = function (powHelper opMul)

powHelper ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > TInt > TNat) (s > TInt)
powHelper mul = unname @2 (powHelper' mul)

powHelper' ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > N "b" TInt > N "n" TNat) (s > TInt)
powHelper' mul =
  begin
    # pick @"n"
    # ifZero
      (int 1 # del @"n" # del @"b")
      ( begin
          # (pick @"n" # isEven)
          # opIf
            (roll @"b" # roll @"n" # nat 2 # opDiv # pow # square mul)
            (pick @"b" # roll @"b" # roll @"n" # op1 # opSubUnsafe # pow # mul)
      )
  where
    square ::
      (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
      FN (s > TInt) (s > TInt)
    square mul' = opDup # mul'
