-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Alba.Dsl.V1.Bch2026.Contract.VectorAlgorithms where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    Stack ((:>)),
    StackEntry,
    TNat,
    TQuotA,
    del,
    i2nUnsafe,
    int,
    invoke1,
    n2i,
    name,
    name2,
    ns,
    ns3,
    ns4,
    ns5,
    opBoolAnd,
    opFalse,
    opIf,
    opLessThanOrEqual,
    opTrue,
    opUntil,
    pick,
    pickN,
    quot0,
    quot1,
    roll,
    rollN,
    un,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Conditionals (cond)
import Alba.Dsl.V1.Bch2026.Contract.Error (errCanNotHappen)
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Misc (do')
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip, swap)
import Alba.Dsl.V1.Bch2026.Contract.TInt16 (int16)
import Alba.Dsl.V1.Bch2026.Contract.TMaybe (TMaybe, fromMaybe')
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.Contract.TVectorUnsafe qualified as V
import Prelude ()

-- Merge two vectors that are both sorted in ascending order to produce a new
-- vector sorted in ascending order.
merge ::
  forall a s.
  (PackFs a) =>
  Fn (s :> TQuotA '[a] '[TNat] :> TVector a :> TVector a) (s :> TVector a)
merge = V.empty . opUntil loop . nip . nip . nip
  where
    loop ::
      Loop (s :> TQuotA '[a] '[TNat] :> TVector a :> TVector a :> TVector a)
    loop =
      ns4 #key #vecA #vecB #vecC
        . pick #key
        . name2 #emptyA #emptyB (pick #vecA . V.null . pick #vecB . V.null)
        . cond
          [ ( pick #emptyA . pick #emptyB . opBoolAnd,
              (del #key . del #emptyA . del #emptyB . roll #vecA . roll #vecB)
                . (roll #vecC . opTrue)
            ),
            ( pick #emptyA,
              (del #key . del #emptyA . del #emptyB . roll #vecA . roll #vecB)
                . (V.unconsUnsafe . swap . ns #b . roll #vecC)
                . (roll #b . V.snoc . opFalse)
            ),
            ( pick #emptyB,
              (del #key . del #emptyA . del #emptyB . roll #vecA)
                . (V.unconsUnsafe . swap . ns #a . roll #vecB . roll #vecC)
                . (roll #a . V.snoc . opFalse)
            )
          ]
          ( (del #emptyA . del #emptyB)
              . name2 #a #vecA' (pick #vecA . V.unconsUnsafe)
              . name2 #b #vecB' (pick #vecB . V.unconsUnsafe)
              . (pickN #a . pick #key . un #a . invoke1)
              . (pickN #b . roll #key . un #b . invoke1 . opLessThanOrEqual)
              . opIf
                ( (del #vecA . del #vecB' . del #b . roll #vecA' . roll #vecB)
                    . (roll #vecC . roll #a . V.snoc)
                )
                ( (del #vecA' . del #vecB . del #a . roll #vecA . roll #vecB')
                    . (roll #vecC . roll #b . V.snoc)
                )
              . opFalse
          )

countingSortDesc ::
  forall a s.
  (PackFs a) =>
  Fn (s :> TNat :> TQuotA '[a] '[TNat] :> TVector a) (s :> TVector a)
countingSortDesc =
  ( ns3 #w #key #vec
      . name #n (pick #vec . V.length)
      . name #counts (pick #w . int16 0 . V.replicate)
      . (pick #key . pick #vec . roll #counts . pick #n . n2i . int 0)
      . ( do'
            ( ns4 #key #vec #counts #i
                . (pick #key . roll #vec)
                . (dup . pick #i . i2nUnsafe . V.lookup . fromJust . ns #val)
                . (roll #key . un #val . invoke1)
                . (quot1 add1 . swap . roll #counts . V.adjust . fromJust)
                . (roll #i . int 1)
            )
            . (nip . nip)
        )
      . (int16 0 . int (-1) . roll #w . n2i . sub1)
      . ( do'
            ( ns3 #counts #acc #i
                . (pick #counts . pick #i . i2nUnsafe . V.lookup . fromJust)
                . (pick #i . i2nUnsafe . pick #acc . roll #counts)
                . (V.updateElem . fromJust . swap . roll #acc . add . roll #i)
                . int (-1)
            )
        )
      . (drop . ns #counts' . roll #key . pick #vec . roll #counts' . roll #vec)
      . (roll #n . n2i . int 0)
      . ( do'
            ( ns5 #key #vec #counts #out #i
                . (pick #key . pick #vec)
                . (name #val)
                  (roll #vec . pick #i . i2nUnsafe . V.lookup . fromJust)
                . name #k (pickN #val . roll #key . un #val . invoke1)
                . name #off (pick #counts . pick #k . V.lookup . fromJust)
                . ( (roll #k . toInt . i2nUnsafe . pick #off . add1)
                      . (roll #counts . V.updateElem . fromJust)
                  )
                . ( (roll #off . toInt . i2nUnsafe . rollN #val . roll #out)
                      . (un #val . V.updateElem . fromJust)
                  )
                . (roll #i . int 1)
            )
        )
      . (nip . nip . nip)
  )

fromJust :: (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
fromJust = quot0 (errCanNotHappen) . swap . fromMaybe'
