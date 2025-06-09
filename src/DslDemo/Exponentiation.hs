module DslDemo.Exponentiation (pow) where

import Alba.Dsl.V1.Bch2025.Contract.Math (isEven)
import Alba.Dsl.V1.Bch2026

pow :: FN (s > TInt > TNat) (s > TInt)
pow =
  begin
    # function "pow" (powHelper opMul)
    # invoke "pow" (powHelper opMul)

powHelper ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > TInt > TNat) (s > TInt)
powHelper mul = unname @2 (powHelper' mul)

powHelper' ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > N "b" TInt > N "n" TNat) (s > TInt)
powHelper' mul =
  begin
    # argPick @"n"
    # ifZero
      (int 1 # argsDrop @2)
      ( begin
          # (argPick @"n" # isEven)
          # opIf
            ( begin
                # ( begin
                      # argRoll @"b"
                      # (argRoll @"n" # nat 2 # opDiv)
                      # invoke "pow" (powHelper mul)
                  )
                # square mul
            )
            ( begin
                # argPick @"b"
                # ( begin
                      # argRoll @"b"
                      # (argRoll @"n" # nat 1 # opSubUnsafe)
                      # invoke "pow" (powHelper mul)
                  )
                # mul
            )
      )
  where
    square ::
      (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
      FN (s > TInt) (s > TInt)
    square mul' = opDup # mul'
