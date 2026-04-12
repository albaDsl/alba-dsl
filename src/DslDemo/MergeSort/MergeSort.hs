-- Copyright (c) 2025 albaDsl

module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2025
  ( Fn,
    StackEntry,
    TBool,
    TInt,
    begin,
    cast,
    del,
    name,
    name2,
    nat,
    ns2,
    ns3,
    opBin2Num,
    opFalse,
    opIf,
    opTrue,
    pick,
    roll,
    un,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( PackFs (..),
    TMaybe,
    TPackFs,
    div,
    drop,
    errCanNotHappen,
    fromMaybe',
    greaterThan,
    lessThanOrEqual,
    rot,
    swap,
    untuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.TVector
  ( TVector,
    consF,
    empty,
    lengthF,
    null,
    splitAtF,
    unconsF,
  )
import Alba.Dsl.V1.Bch2026.Lang (fn, lambda0)
import Prelude ()

sort :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TVector a)
sort = packFsRec @a . swap . sortF

sortF :: (StackEntry a) => Fn (s > TPackFs a > TVector a) (s > TVector a)
sortF =
  fn
    ( begin
        . ns2 #pfs #vec
        . (pick #pfs . pick #vec . lengthF . nat 1 . greaterThan)
        . opIf
          ( begin
              . (pick #pfs . roll #vec . halveF)
              . (pick #pfs . swap . sortF . swap)
              . (pick #pfs . swap . sortF . swap)
              . (pick #pfs . rot . rot . mergeF)
          )
          (roll #vec)
        . del #pfs
    )

halveF :: Fn (s > TPackFs a > TVector a) (s > TVector a > TVector a)
halveF =
  fn
    ( begin
        . (ns2 #pfs #vec . pick #pfs . pick #vec . lengthF . nat 2 . div)
        . (roll #pfs . swap . roll #vec . splitAtF)
    )

mergeF ::
  (StackEntry a) =>
  Fn (s > TPackFs a > TVector a > TVector a) (s > TVector a)
mergeF =
  fn
    ( begin
        . (ns3 #pfs #xs #ys . pick #xs . pick #ys . baseCases)
        . opIf
          ( begin
              . drop
              . name2 #x #xRest (pick #pfs . pick #xs . uncons')
              . name2 #y #yRest (pick #pfs . pick #ys . uncons')
              . ( begin
                    . (pick #x . toNum . pick #y . toNum)
                    . lessThanOrEqual
                )
              . opIf
                ( begin
                    . (name #elem (roll #x) . roll #xRest . roll #ys)
                    . (del #yRest . del #y . del #xs)
                )
                ( begin
                    . (name #elem (roll #y) . roll #xs . roll #yRest)
                    . (del #ys . del #xRest . del #x)
                )
              . (pick #pfs . rot . rot . mergeF)
              . (roll #pfs . rot . rot . un #elem . consF)
          )
          (del #pfs . del #xs . del #ys)
    )
  where
    uncons' ::
      (StackEntry a) =>
      Fn (s' > TPackFs a > TVector a) (s' > a > TVector a)
    uncons' = unconsF . fromJust . untuple

    -- FIXME: cast.
    toNum :: Fn (s' > a) (s' > TInt)
    toNum = cast . opBin2Num

    baseCases :: Fn (s' > TVector a > TVector a) (s' > TVector a > TBool)
    baseCases =
      begin
        . (ns2 #xs #ys . pick #xs . null)
        . opIf
          (roll #ys . del #xs . opFalse)
          ( begin
              . (pick #ys . null)
              . opIf
                (del #ys . roll #xs . opFalse)
                (del #xs . del #ys . empty . opTrue)
          )

-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => Fn (s > TMaybe a) (s > a)
fromJust = lambda0 (errCanNotHappen) . swap . fromMaybe'
