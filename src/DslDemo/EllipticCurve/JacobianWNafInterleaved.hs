-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.JacobianWNafInterleaved
  ( ecMulInterleaved,
    ecDouble,
    ecAdd,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    StackEntry,
    TInt,
    TNat,
    TQuotA,
    TQuotB,
    cast,
    del,
    i2n,
    n2i,
    name,
    name2,
    name3,
    nat,
    ns2,
    opIf,
    opRShiftNum,
    pick,
    quot0,
    quot1,
    quot2,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026 qualified as Bch
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( Integral (add, fromInt, sub, toInt),
    TInt16,
    TInt264,
    TMaybe,
    TTuple,
    apply2,
    errCanNotHappen,
    fromMaybe',
    ifJust,
    ifZero,
    just,
    nothing,
    tuple,
    untuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand
import Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances ()
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.Contract.VectorAlgorithms (merge)
import Alba.Dsl.V1.Bch2026.QuotationsB qualified as QB
import DslDemo.EllipticCurve.Common (countTrailingZeros, doubleN, mods)
import DslDemo.EllipticCurve.Jacobian (ecAdd, ecDouble)
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import Prelude (Int)

type TScalarAndLookup = TTuple TInt264 (TQuotB '[TInt16] '[TPointJ])

type TTerm = TTuple TInt16 TInt16

type TTerm' = TTuple (TTuple TInt16 TInt16) (TQuotB '[TInt16] '[TPointJ])

windowSize :: Int
windowSize = 5

fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
fromJust = quot0 errCanNotHappen . swap . fromMaybe'

ecMulInterleaved :: Bch.Fn (s :> V.TVector TScalarAndLookup) (s :> TPointJ)
ecMulInterleaved =
  (name #sorted sorted . pick #sorted . V.null)
    . (opIf (del #sorted . makeIdentity))
      ( name #pHi (pick #sorted . V.last . fromJust . posOf)
          . (quot2 combine . makeIdentity . roll #pHi . n2Int16 . tuple)
          . (roll #sorted . V.foldr . untuple . toInt . i2n . swap . doubleN)
      )
  where
    combine ::
      Fn (s :> TTerm' :> TTuple TPointJ TInt16) (s :> TTuple TPointJ TInt16)
    combine =
      ns2 #term #tup
        . name2 #acc #prevPos (roll #tup . untuple)
        . name3 #lookup #d #pos (roll #term . untuple . swap . untuple)
        . (roll #prevPos . pick #pos . sub . toInt . i2n . roll #acc . doubleN)
        . (roll #d . roll #lookup . QB.invoke1 . EC.ecAddJ . roll #pos . tuple)

    n2Int16 :: Fn (s :> TNat) (s :> TInt16)
    n2Int16 = n2i . fromInt

sorted :: Fn (s :> V.TVector TScalarAndLookup) (s :> TVector TTerm')
sorted =
  V.uncons
    . ifJust
      (untuple . swap . taggedTerms . quot2 (step) . rot . rot . swap . V.foldl)
      V.empty
  where
    step :: Fn (s :> TVector TTerm' :> TScalarAndLookup) (s :> TVector TTerm')
    step = taggedTerms . quot1 posOf . rot . rot . merge

    taggedTerms :: Fn (s :> TScalarAndLookup) (s :> TVector TTerm')
    taggedTerms =
      (untuple . ns2 #n #lookup . roll #lookup . conv)
        . (roll #n . terms . V.map)

    conv ::
      Fn
        (s :> TQuotB '[TInt16] '[TPointJ])
        (s :> TQuotA '[TTerm] '[TTerm'])
    conv = quot2 tuple . apply2

posOf :: Fn (s :> TTerm') (s :> TNat)
posOf = untuple . drop . untuple . nip . toInt . fromInt

-- In LSB first (ascending) order.
terms :: Fn (s :> TInt264) (s :> TVector TTerm)
terms = nat 0 . tuple . quot1 step . swap . V.unfoldr
  where
    step ::
      Fn
        (s :> TTuple TInt264 TNat)
        (s :> TMaybe (TTuple TTerm (TTuple TInt264 TNat)))
    step =
      (untuple . ns2 #m #base . pick #m . toInt)
        . (ifZero (del #base . del #m . nothing))
          ( (name #z (pick #m . toInt . countTrailingZeros))
              . (name #m' (roll #m . toInt . pick #z . opRShiftNum))
              . (name #pos (roll #base . roll #z . add))
              . (name #d (pick #m' . mods windowSize))
              . ( (pick #d . fromInt . pick #pos . n2i . fromInt . tuple)
                    . (roll #m' . roll #d . sub . i2Int264 . roll #pos . tuple)
                    . (tuple . just)
                )
          )

    i2Int264 :: Fn (s :> TInt) (s :> TInt264)
    i2Int264 = cast
