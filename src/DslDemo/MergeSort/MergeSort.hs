-- Copyright (c) 2025 albaDsl

module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2026
import Prelude hiding (drop, null)

sort :: FN (s > TBytes) (s > TBytes)
sort = function (unname @1 sort')
  where
    sort' :: FN (s > N "xs" TBytes) (s > TBytes)
    sort' =
      begin
        # (pick @"xs" # opSize # opNip)
        # (opDup # nat 0 # opNumEqual # opSwap # nat 1 # opNumEqual # opBoolOr)
        # opIf
          (roll @"xs")
          ( begin
              # name2 @"fst" @"snd" (roll @"xs" # halve)
              # (roll @"fst" # sort # roll @"snd" # sort # merge)
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
    # (pickN @"xs" # pickN @"ys" # baseCases)
    # opIf
      ( begin
          # opDrop
          # name2 @"x" @"xRest" (pick @"xs" # uncons)
          # name2 @"y" @"yRest" (pick @"ys" # uncons)
          # ( begin
                # (pick @"x" # opBin2Num)
                # (pick @"y" # opBin2Num)
                # opLessThanOrEqual
            )
          # opIf
            ( begin
                # (roll @"x" # roll @"xRest" # roll @"ys")
                # (drop @"yRest" # drop @"y" # drop @"xs")
            )
            ( begin
                # (roll @"y" # roll @"xs" # roll @"yRest")
                # (drop @"ys" # drop @"xRest" # drop @"x")
            )
          # (merge # opCat)
      )
      (drop @"xs" # drop @"ys")
  where
    baseCases :: FN (s > N "xs" TBytes > N "ys" TBytes) (s > TBytes > TBool)
    baseCases =
      begin
        # (pick @"xs" # null)
        # opIf
          (roll @"ys" # drop @"xs" # opFalse)
          ( begin
              # (pick @"ys" # null)
              # opIf
                (drop @"ys" # roll @"xs" # opFalse)
                (drop @"xs" # drop @"ys" # bytes [] # opTrue)
          )
