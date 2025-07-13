-- Copyright (c) 2025 albaDsl

module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2026
import Prelude hiding (drop)

sort :: FN (s > TBytes) (s > TBytes)
sort = function (unname @1 sort')
  where
    sort' :: FN (s > N "xs" TBytes) (s > TBytes)
    sort' =
      begin
        # name @"size" (pick @"xs" # opSize # opNip)
        # pick @"size"
        # ifZero
          (drop @"size" # roll @"xs")
          ( begin
              # (roll @"size" # nat 1 # opEqual)
              # opIf
                (roll @"xs")
                ( begin
                    # name2 @"fst" @"snd" (roll @"xs" # halve)
                    # (roll @"fst" # sort # roll @"snd" # sort # merge)
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
    # (pick @"xs" # opSize # opNip)
    # ifZero
      (roll @"ys" # drop @"xs")
      ( begin
          # (pick @"ys" # opSize # opNip)
          # ifZero
            (drop @"ys" # roll @"xs")
            ( begin
                # name2 @"x" @"xRest" (pick @"xs" # uncons)
                # name2 @"y" @"yRest" (pick @"ys" # uncons)
                # ( begin
                      # (pick @"x" # opBin2Num)
                      # (pick @"y" # opBin2Num)
                      # opLessThanOrEqual
                  )
                # opIf
                  ( begin
                      # (roll @"x" # roll @"xRest" # roll @"ys" # merge # opCat)
                      # (drop @"yRest" # drop @"y" # drop @"xs")
                  )
                  ( begin
                      # (roll @"y" # roll @"xs" # roll @"yRest" # merge # opCat)
                      # (drop @"ys" # drop @"xRest" # drop @"x")
                  )
            )
      )
