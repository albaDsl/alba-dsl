-- Copyright (c) 2025 albaDsl

module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2026
  ( FN,
    StackEntry,
    TBool,
    TInt,
    begin,
    bytes,
    cast,
    castStack,
    drop,
    name2,
    nat,
    ns2,
    ns3,
    opBin2Num,
    opDiv,
    opDrop,
    opFalse,
    opGreaterThan,
    opIf,
    opLessThanOrEqual,
    opRot,
    opSwap,
    opTrue,
    opVerify,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Hide (Hide, nipHide)
import Alba.Dsl.V1.Bch2026.Contract.Maybe (TMaybe, fromMaybe')
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs, TPackFs, packFs)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (untuple)
import Alba.Dsl.V1.Bch2026.Contract.Vector
  ( TVector,
    consF,
    empty,
    lengthF,
    null,
    splitAtF,
    unconsF,
  )
import Alba.Dsl.V1.Bch2026.Lang (function, lambda0)
import Prelude ()

sort :: forall a s. (PackFs a) => FN (s > TVector a) (s > TVector a)
sort = packFs @a # opSwap # sortF

sortF :: (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TVector a)
sortF =
  function
    ( begin
        # ns2 @"pfs" @"vec"
        # (pick @"pfs" # pick @"vec" # lengthF # nat 1 # opGreaterThan)
        # opIf
          ( begin
              # (pick @"pfs" # roll @"vec" # halveF)
              # (pick @"pfs" # opSwap # sortF # opSwap)
              # (pick @"pfs" # opSwap # sortF # opSwap)
              # (pick @"pfs" # opRot # opRot # mergeF)
          )
          (roll @"vec")
        # drop @"pfs"
    )

halveF :: FN (s > TPackFs a > TVector a) (s > TVector a > TVector a)
halveF =
  function
    ( begin
        # ns2 @"pfs" @"vec"
        # (pick @"pfs" # pick @"vec" # lengthF # nat 2 # opDiv)
        # (roll @"pfs" # opSwap # roll @"vec" # splitAtF)
    )

mergeF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TVector a > TVector a) (s > TVector a)
mergeF =
  function
    ( begin
        # (ns3 @"pfs" @"xs" @"ys" # pick @"xs" # pick @"ys" # baseCases)
        # opIf
          ( begin
              # opDrop
              # name2 @"x" @"xRest" (pick @"pfs" # pick @"xs" # uncons')
              # name2 @"y" @"yRest" (pick @"pfs" # pick @"ys" # uncons')
              # ( begin
                    # (pick @"x" # toNum # pick @"y" # toNum)
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
              # (pick @"pfs" # opRot # opRot # mergeF)
              # (roll @"pfs" # opRot # opRot # nipHide # consF)
          )
          (drop @"pfs" # drop @"xs" # drop @"ys")
    )
  where
    uncons' ::
      (StackEntry a) =>
      FN (s' > TPackFs a > TVector a) (s' > Hide a > TVector a)
    uncons' = unconsF # fromJust # untuple # castStack

    toNum :: FN (s' > Hide a) (s' > TInt)
    toNum = cast # opBin2Num

    baseCases :: FN (s' > TVector a > TVector a) (s' > TVector a > TBool)
    baseCases =
      begin
        # (ns2 @"xs" @"ys" # pick @"xs" # null)
        # opIf
          (roll @"ys" # drop @"xs" # opFalse)
          ( begin
              # (pick @"ys" # null)
              # opIf
                (drop @"ys" # roll @"xs" # opFalse)
                (drop @"xs" # drop @"ys" # empty # opTrue)
          )

-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => FN (s > TMaybe a) (s > a)
fromJust = err # opSwap # fromMaybe'
  where
    err = lambda0 (bytes "E0" # opFalse # opVerify # cast)
