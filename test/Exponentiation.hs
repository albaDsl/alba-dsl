module Exponentiation (pow) where

import Alba.Dsl.V1.Bch2026

pow :: FN (s > TInt > TNat) (s > TInt)
pow = lambda' powHelper # recur powHelper

powHelper :: FN (s > TInt > TNat > TLambdaUntyped) (s > TInt)
powHelper = unname @3 powHelper'

powHelper' ::
  FN (s > N "b" TInt > N "n" TNat > N "rec" TLambdaUntyped) (s > TInt)
powHelper' =
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
                      # recur powHelper
                  )
                # square
                # argsDrop @3
            )
            ( begin
                # argPick @"b"
                # ( begin
                      # argPick @"b"
                      # (argPick @"n" # nat 1 # opSubUnsafe)
                      # argPick @"rec"
                      # recur powHelper
                  )
                # opMul
                # argsDrop @3
            )
      )

isEven :: FN (s > TNat) (s > TBool)
isEven = natToInt # int 2 # opMod # int 0 # opEqual

square :: FN (s > TInt) (s > TInt)
square = opDup # opMul
