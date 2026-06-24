-- Copyright (c) 2025 albaDsl

module DslDemo.MergeSort.MergeSort where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBool,
    begin,
    del,
    fn,
    invoke2,
    quot0,
    name,
    name2,
    nat,
    ns2,
    ns3,
    ns4,
    opFalse,
    opIf,
    opRoll,
    opTrue,
    pick,
    pickN,
    roll,
    un,
    un2,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( Ord,
    PackFs (..),
    TMaybe,
    TOrdRec,
    TPackFs,
    div,
    drop,
    errCanNotHappen,
    fromMaybe',
    getLessThanOrEqual,
    greaterThan,
    ordRec,
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
import Prelude ()

sort :: forall a s. (Ord a, PackFs a) => Fn (s :> TVector a) (s :> TVector a)
sort = ordRec @a . packFsRec @a . rot . sortF

sortF ::
  (StackEntry a) =>
  Fn (s :> TOrdRec a :> TPackFs a :> TVector a) (s :> TVector a)
sortF =
  fn
    ( begin
        . ns3 #ord #pfs #vec
        . (pick #pfs . pick #vec . lengthF . nat 1 . greaterThan)
        . opIf
          ( begin
              . (pick #pfs . roll #vec . halveF)
              . (pick #ord . pick #pfs . rot . sortF . swap)
              . (pick #ord . pick #pfs . rot . sortF . swap)
              . (pick #ord . pick #pfs . opRoll 3 . opRoll 3 . mergeF)
          )
          (roll #vec)
        . (del #pfs . del #ord)
    )

halveF :: Fn (s :> TPackFs a :> TVector a) (s :> TVector a :> TVector a)
halveF =
  fn
    ( begin
        . (ns2 #pfs #vec . pick #pfs . pick #vec . lengthF . nat 2 . div)
        . (roll #pfs . swap . roll #vec . splitAtF)
    )

mergeF ::
  (StackEntry a) =>
  Fn (s :> TOrdRec a :> TPackFs a :> TVector a :> TVector a) (s :> TVector a)
mergeF =
  fn
    ( begin
        . (ns4 #ord #pfs #xs #ys . pick #xs . pick #ys . baseCases)
        . opIf
          ( begin
              . drop
              . name2 #x #xRest (pick #pfs . pick #xs . uncons')
              . name2 #y #yRest (pick #pfs . pick #ys . uncons')
              . ( begin
                    . (pickN #x . pickN #y . pick #ord . un2 #x #y)
                    . (getLessThanOrEqual . invoke2)
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
              . (roll #ord . pick #pfs . opRoll 3 . opRoll 3 . mergeF)
              . (roll #pfs . rot . rot . un #elem . consF)
          )
          (del #ys . del #xs . del #pfs . del #ord)
    )
  where
    uncons' ::
      (StackEntry a) =>
      Fn (s' :> TPackFs a :> TVector a) (s' :> a :> TVector a)
    uncons' = unconsF . fromJust . untuple

    baseCases :: Fn (s' :> TVector a :> TVector a) (s' :> TVector a :> TBool)
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
fromJust :: (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
fromJust = quot0 (errCanNotHappen) . swap . fromMaybe'
