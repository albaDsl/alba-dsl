module RecursionExamples.Exponentiation (pow, pow') where

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
                      # argPick @"b"
                      # (argPick @"n" # nat 2 # opDiv)
                      # argPick @"rec"
                      # recur (powHelper mul)
                  )
                # square mul
                # argsDrop @3
            )
            ( begin
                # argPick @"b"
                # ( begin
                      # argPick @"b"
                      # (argPick @"n" # nat 1 # opSubUnsafe)
                      # argPick @"rec"
                      # recur (powHelper mul)
                  )
                # mul
                # argsDrop @3
            )
      )
  where
    isEven :: FN (s > TNat) (s > TBool)
    isEven = natToInt # int 2 # opMod # int 0 # opNumEqual

    square ::
      (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
      FN (s > TInt) (s > TInt)
    square mul' = opDup # mul'
