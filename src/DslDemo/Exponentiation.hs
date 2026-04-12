-- Copyright (c) 2025 albaDsl

module DslDemo.Exponentiation (pow) where

import Alba.Dsl.V1.Bch2025.Contract.Prelude (ifZero, isEven, nat1SubUnsafe)
import Alba.Dsl.V1.Bch2026

pow :: Fn (s > TInt > TNat) (s > TInt)
pow = fn (powHelper opMul)

powHelper ::
  (forall s'. Fn (s' > TInt > TInt) (s' > TInt)) ->
  Fn (s > TInt > TNat) (s > TInt)
powHelper mul = unname 2 (powHelper' mul)

powHelper' ::
  (forall s'. Fn (s' > TInt > TInt) (s' > TInt)) ->
  Fn (s > N "b" TInt > N "n" TNat) (s > TInt)
powHelper' mul =
  begin
    # pick "n"
    # ifZero
      (int 1 # del "n" # del "b")
      ( begin
          # (pick "n" # isEven)
          # opIf
            (roll "b" # roll "n" # nat 2 # opDiv # pow # square mul)
            (pick "b" # roll "b" # roll "n" # nat1SubUnsafe # pow # mul)
      )
  where
    square ::
      (forall s'. Fn (s' > TInt > TInt) (s' > TInt)) ->
      Fn (s > TInt) (s > TInt)
    square mul' = opDup # mul'
