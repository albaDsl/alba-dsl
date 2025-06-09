module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2026

setup :: FNC
setup =
  begin
    # function "msort" msort
    # function "merge" merge

sort :: FN (s > TBytes) (s > TBytes)
sort = invoke "msort" msort

msort :: FN (s > TBytes) (s > TBytes)
msort = unname @1 msort'

msort' :: FN (s > N "xs" TBytes) (s > TBytes)
msort' =
  begin
    # name @"size" (argPick @"xs" # opSize # opNip)
    # argPick @"size"
    # ifZero
      (argRoll @"xs" # argsDrop @1)
      ( begin
          # (argRoll @"size" # nat 1 # opEqual)
          # opIf
            (argRoll @"xs")
            ( begin
                # name2 @"fst" @"snd" (argRoll @"xs" # halve)
                # (argRoll @"fst" # invoke "msort" msort)
                # (argRoll @"snd" # invoke "msort" msort)
                # invoke "merge" merge
            )
      )

halve :: FN (s > TBytes) (s > TBytes > TBytes)
halve = opSize # op2 # opDiv # opSplit

uncons :: FN (s > TBytes) (s > TBytes > TBytes)
uncons = nat 1 # opSplit

merge :: FN (s > TBytes > TBytes) (s > TBytes)
merge = unname @2 merge'

merge' :: FN (s > N "xs" TBytes > N "ys" TBytes) (s > TBytes)
merge' =
  begin
    # (argPick @"xs" # opSize # opNip)
    # ifZero
      (argRoll @"ys" # argsDrop @1)
      ( begin
          # (argPick @"ys" # opSize # opNip)
          # ifZero
            (argRoll @"xs" # argsDrop @1)
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
                      # argRoll @"x"
                      # ( begin
                            # argRoll @"xRest"
                            # argRoll @"ys"
                            # invoke "merge" merge
                        )
                      # argsDrop @3
                      # opCat
                  )
                  ( begin
                      # argRoll @"y"
                      # ( begin
                            # argRoll @"xs"
                            # argRoll @"yRest"
                            # invoke "merge" merge
                        )
                      # argsDrop @3
                      # opCat
                  )
            )
      )
