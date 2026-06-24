-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianWindowed
  ( TTable,
    setupTableM,
    ecDouble,
    ecAdd,
    ecMul4,
    ecMul6,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    StackEntry,
    TInt,
    TNat,
    begin,
    del,
    fn,
    i2nUnsafe,
    n2i,
    name,
    nat,
    ns2,
    ns4,
    op0,
    op2Drop,
    opFalse,
    opIf,
    opTrue,
    opUntil,
    pick,
    quot0,
    quot2,
    quot4,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (equal),
    Integral (div, fromInt, mod, toInt),
    Ord (greaterThan),
    TInt8,
    TMaybe,
    TTuple,
    apply2,
    apply3,
    apply4,
    dup,
    errCanNotHappen,
    fromMaybe',
    ifZero,
    just,
    nat1SubUnsafe,
    nip,
    nothing,
    swap,
    tuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import DslDemo.EllipticCurve.Jacobian
  ( ecAdd,
    ecDouble,
    fromJacobian,
    toJacobian,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import DslDemo.EllipticCurve.Point (TPoint)
import Numeric.Natural (Natural)
import Prelude ((^))

type TTable = TVector TPointJ

setupTableM :: Natural -> Fn (s :> TPoint) (s :> TTable)
setupTableM windowSize =
  (toJacobian . quot2 EC.ecAddJ . apply2 . nat numValues . swap)
    . (makeIdentity . V.iterateN)
  where
    numValues = 2 ^ windowSize

ecMul4 :: Fn (s :> TTable :> TNat) (s :> TPoint)
ecMul4 = fn (ecMulM 4)

ecMul6 :: Fn (s :> TTable :> TNat) (s :> TPoint)
ecMul6 = fn (ecMulM 6)

ecMulM :: Natural -> Fn (s :> TTable :> TNat) (s :> TPoint)
ecMulM windowSize =
  begin
    . (ns2 #tab #n . pick #n . nat 0 . equal)
    . (opIf (del #tab . del #n . makeIdentity))
      ( begin
          . (roll #tab . nat windowSize . quot4 f . apply4 . apply3)
          . (makeIdentity . roll #n . digitsM windowSize . V.foldr)
      )
    . fromJacobian
  where
    f :: Fn (s :> TInt8 :> TPointJ :> TTable :> TNat) (s :> TPointJ)
    f =
      ( begin
          . (ns4 #digit #q #tab #numVals)
          . (name #q' (roll #numVals . roll #q . doubleN))
          . (pick #digit . toInt . op0 . greaterThan)
          . opIf
            ( begin
                . (roll #tab . roll #digit . toInt . i2nUnsafe)
                . (V.lookup . fromJust . roll #q' . EC.ecAddJ)
            )
            (del #tab . del #digit . roll #q')
      )

    fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
    fromJust = quot0 (errCanNotHappen) . swap . fromMaybe'

doubleN :: Fn (s :> TNat :> TPointJ) (s :> TPointJ)
doubleN =
  opUntil
    ( begin
        . (swap . dup . op0 . equal)
        . opIf (swap . opTrue) (nat1SubUnsafe . swap . EC.ecDoubleJ . opFalse)
    )
    . nip

digitsM :: Natural -> Fn (s :> TNat) (s :> V.TVector TInt8)
digitsM windowSize = nat numValues . quot2 f . apply2 . swap . V.unfoldr
  where
    numValues = 2 ^ windowSize

    f :: Fn (s :> TNat :> TNat) (s :> TMaybe (TTuple TInt8 TNat))
    f = swap . dup . ifZero (op2Drop . nothing) (swap . tup . just)

    tup :: Fn (s :> TNat :> TNat) (s :> TTuple TInt8 TNat)
    tup =
      begin
        . ns2 #val #numVals
        . (pick #val . pick #numVals . mod . n2i . conv)
        . (roll #val . roll #numVals . div . tuple)

    conv :: Fn (s :> TInt) (s :> TInt8)
    conv = fromInt
