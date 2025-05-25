module RecursionExamples.MergeSort where

import Alba.Dsl.V1.Bch2026

sort :: FN (s > TBytes) (s > TBytes)
sort = lambda' merge # lambda' msort # recur msort

msort :: FN (s > TBytes > TLambdaUntyped > TLambdaUntyped) (s > TBytes)
msort = unname @3 msort'

msort' ::
  FN
    (s > N "xs" TBytes > N "merge" TLambdaUntyped > N "rec" TLambdaUntyped)
    (s > TBytes)
msort' =
  begin
    # name @"size" (argPick @"xs" # opSize # opNip)
    # argPick @"size"
    # ifZero
      (argRoll @"xs" # argsDrop @3)
      ( begin
          # (argPick @"size" # nat 1 # opEqual)
          # opIf
            (argRoll @"xs" # argsDrop @3)
            ( begin
                # name2 @"fst" @"snd" (argRoll @"xs" # halve)
                # ( argRoll @"fst"
                      # argPick @"merge"
                      # argPick @"rec"
                      # recur msort
                  )
                # ( argRoll @"snd"
                      # argPick @"merge"
                      # argRoll @"rec"
                      # recur msort
                  )
                # (argRoll @"merge")
                # argsDrop @1
                # recur merge
            )
      )

halve :: FN (s > TBytes) (s > TBytes > TBytes)
halve = opSize # op2 # opDiv # opSplit

uncons :: FN (s > TBytes) (s > TBytes > TBytes)
uncons = nat 1 # opSplit

merge :: FN (s > TBytes > TBytes > TLambdaUntyped) (s > TBytes)
merge = unname @3 merge'

merge' ::
  FN
    (s > N "xs" TBytes > N "ys" TBytes > N "rec" TLambdaUntyped)
    (s > TBytes)
merge' =
  begin
    # (argPick @"xs" # opSize # opNip)
    # ifZero
      (argRoll @"ys" # argsDrop @2)
      ( begin
          # (argPick @"ys" # opSize # opNip)
          # ifZero
            (argRoll @"xs" # argsDrop @2)
            ( begin
                # name2 @"x" @"xRest" (argPick @"xs" # uncons)
                # name2 @"y" @"yRest" (argPick @"ys" # uncons)
                # ( begin
                      # (argPick @"x" # opBin2Num)
                      # (argPick @"y" # opBin2Num)
                      # opLessThanOrEqual
                  )
                # opIf
                  ( begin
                      # argPick @"x"
                      # ( argRoll @"xRest"
                            # argRoll @"ys"
                            # argRoll @"rec"
                            # recur merge
                        )
                      # argsDrop @4
                      # opCat
                  )
                  ( begin
                      # argPick @"y"
                      # ( argRoll @"xs"
                            # argRoll @"yRest"
                            # argRoll @"rec"
                            # recur merge
                        )
                      # argsDrop @4
                      # opCat
                  )
            )
      )
