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
    i2nUnsafe,
    n2i,
    name,
    name2,
    name3,
    nat,
    ns2,
    ns3,
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
    fst,
    ifJust,
    ifZero,
    just,
    nothing,
    snd,
    tuple,
    untuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand
import Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances ()
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.Contract.VectorAlgorithms (merge)
import Alba.Dsl.V1.Bch2026.QuotationsB qualified as QB
import DslDemo.EllipticCurve.Common (countTrailingZeros, doubleN, mods')
import DslDemo.EllipticCurve.Jacobian (ecAdd, ecDouble)
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import DslDemo.EllipticCurve.Point (TPoint)
import Prelude ()

type TWindowSize = TInt16

type TMsmTerm = TTuple (TTuple TInt264 TWindowSize) (TQuotB '[TInt16] '[TPoint])

type TTerm = TTuple TInt16 TInt16

type TTerm' = TTuple (TTuple TInt16 TInt16) (TQuotB '[TInt16] '[TPoint])

fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
fromJust = quot0 errCanNotHappen . swap . fromMaybe'

-- Multi-scalar interleaved multiplication (MSM).
ecMulInterleaved :: Bch.Fn (s :> V.TVector TMsmTerm) (s :> TPointJ)
ecMulInterleaved =
  (name #sorted sorted . pick #sorted . V.null)
    . (opIf (del #sorted . makeIdentity))
      ( name #pHi (pick #sorted . V.last . fromJust . posOf)
          . (quot2 combine . makeIdentity . roll #pHi . n2TInt16 . tuple)
          . (roll #sorted . V.foldr . untuple . toInt . i2n . swap . doubleN)
      )
  where
    combine ::
      Fn (s :> TTerm' :> TTuple TPointJ TInt16) (s :> TTuple TPointJ TInt16)
    combine =
      ns2 #term #tup
        . name2 #acc #prevPos (roll #tup . untuple)
        . name3 #lookup #d #pos (roll #term . untuple . swap . untuple)
        . (roll #prevPos . pick #pos . sub . toInt . i2nUnsafe . roll #acc)
        . (doubleN . roll #d . roll #lookup . QB.invoke1 . EC.ecAddMixedJ)
        . (roll #pos . tuple)

    n2TInt16 :: Fn (s :> TNat) (s :> TInt16)
    n2TInt16 = n2i . fromInt

sorted :: Fn (s :> V.TVector TMsmTerm) (s :> TVector TTerm')
sorted =
  V.uncons
    . ifJust
      (untuple . swap . taggedTerms . quot2 (step) . rot . rot . swap . V.foldl)
      V.empty
  where
    step :: Fn (s :> TVector TTerm' :> TMsmTerm) (s :> TVector TTerm')
    step = taggedTerms . quot1 posOf . rot . rot . merge

    taggedTerms :: Fn (s :> TMsmTerm) (s :> TVector TTerm')
    taggedTerms = untuple . conv . swap . terms . V.map

    conv ::
      Fn
        (s :> TQuotB '[TInt16] '[TPoint])
        (s :> TQuotA '[TTerm] '[TTerm'])
    conv = quot2 tuple . apply2

posOf :: Fn (s :> TTerm') (s :> TNat)
posOf = fst . snd . toInt . i2nUnsafe

-- In LSB first (ascending) order.
terms :: Fn (s :> TTuple TInt264 TWindowSize) (s :> TVector TTerm)
terms =
  untuple . swap . nat 0 . tuple . swap . quot2 step . apply2 . swap . V.unfoldr
  where
    step ::
      Fn
        (s :> TTuple TInt264 TNat :> TWindowSize)
        (s :> TMaybe (TTuple TTerm (TTuple TInt264 TNat)))
    step =
      (swap . untuple . ns3 #wsize #m #base . pick #m . toInt)
        . (ifZero (del #base . del #m . del #wsize . nothing))
          ( (name #z (pick #m . toInt . countTrailingZeros))
              . (name #m' (roll #m . toInt . pick #z . opRShiftNum))
              . (name #pos (roll #base . roll #z . add))
              . (name #d (pick #m' . roll #wsize . toInt . i2nUnsafe . mods'))
              . ( (pick #d . i2TInt16Unsafe . pick #pos . n2i . i2TInt16Unsafe)
                    . (tuple . roll #m' . roll #d . sub . i2Int264Unsafe)
                    . (roll #pos . tuple . tuple . just)
                )
          )

    i2TInt16Unsafe :: Fn (s :> TInt) (s :> TInt16)
    i2TInt16Unsafe = cast

    i2Int264Unsafe :: Fn (s :> TInt) (s :> TInt264)
    i2Int264Unsafe = cast
