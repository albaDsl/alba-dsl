module DslDemo.Exponentiation (pow, pow') where

import Alba.Dsl.V1.Bch2025.Contract.Math (isEven)
import Alba.Dsl.V1.Bch2026

pow :: FN (s > TInt > TNat) (s > TInt)
pow = lambda' (powHelper opMul) # recur (powHelper opMul)

pow' ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > TInt > TNat) (s > TInt)
pow' mul = lambda' (powHelper mul) # recur (powHelper mul)

powHelper ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > TInt > TNat > TLambdaUntyped) (s > TInt)
powHelper mul = unname @3 (powHelper' mul)

powHelper' ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > N "b" TInt > N "n" TNat > N "rec" TLambdaUntyped) (s > TInt)
powHelper' mul =
  begin
    # argPick @"n"
    # ifZero
      (int 1 # argsDrop @3)
      ( begin
          # (argPick @"n" # isEven)
          # opIf
            ( begin
                # ( begin
                      # argRoll @"b"
                      # (argRoll @"n" # nat 2 # opDiv)
                      # argRoll @"rec"
                      # recur (powHelper mul)
                  )
                # square mul
            )
            ( begin
                # argPick @"b"
                # ( begin
                      # argRoll @"b"
                      # (argRoll @"n" # nat 1 # opSubUnsafe)
                      # argRoll @"rec"
                      # recur (powHelper mul)
                  )
                # mul
            )
      )
  where
    square ::
      (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
      FN (s > TInt) (s > TInt)
    square mul' = opDup # mul'
