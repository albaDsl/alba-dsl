-- Copyright (c) 2025 albaDsl

module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2026

sort :: FN (s > TBytes) (s > TBytes)
sort = function (unname @1 sort')
  where
    sort' :: FN (s > N "xs" TBytes) (s > TBytes)
    sort' =
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
                    # (argRoll @"fst" # sort)
                    # (argRoll @"snd" # sort)
                    # merge
                )
          )

halve :: FN (s > TBytes) (s > TBytes > TBytes)
halve = opSize # op2 # opDiv # opSplit

uncons :: FN (s > TBytes) (s > TBytes > TBytes)
uncons = nat 1 # opSplit

merge :: FN (s > TBytes > TBytes) (s > TBytes)
merge = function (unname @2 merge')

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
                            # merge
                        )
                      # argsDrop @3
                      # opCat
                  )
                  ( begin
                      # argRoll @"y"
                      # ( begin
                            # argRoll @"xs"
                            # argRoll @"yRest"
                            # merge
                        )
                      # argsDrop @3
                      # opCat
                  )
            )
      )
