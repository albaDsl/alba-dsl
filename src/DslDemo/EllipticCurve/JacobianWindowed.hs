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
  ( Env,
    Fn,
    Stack ((:>)),
    StackEntry,
    TNat,
    begin,
    del,
    fn,
    i2nUnsafe,
    lambda0,
    lambda1,
    lambda2,
    lambda3,
    n2i,
    name,
    nat,
    ns2,
    ns3,
    op0,
    opFalse,
    opIf,
    opTrue,
    opUntil,
    pick,
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
    drop,
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

setupTableM :: Natural -> Env (s :> TPoint) (s :> TTable)
setupTableM windowSize =
  (toJacobian . lambda2 EC.ecAddJ . apply2 . nat numValues . swap)
    . (makeIdentity . V.iterateN)
  where
    numValues = 2 ^ windowSize

ecMul4 :: Env (s :> TTable :> TNat) (s :> TPoint)
ecMul4 = fn (ecMulM 4)

ecMul6 :: Env (s :> TTable :> TNat) (s :> TPoint)
ecMul6 = fn (ecMulM 6)

ecMulM :: Natural -> Env (s :> TTable :> TNat) (s :> TPoint)
ecMulM windowSize =
  begin
    . (ns2 #tab #n . pick #n . nat 0 . equal)
    . (opIf (del #tab . del #n . makeIdentity))
      ( begin
          . (roll #tab . lambda3 f . apply3)
          . (makeIdentity . roll #n . digitsM windowSize . V.foldr)
      )
    . fromJacobian
  where
    f :: Fn (s :> TInt8 :> TPointJ :> TTable) (s :> TPointJ)
    f =
      ( begin
          . (ns3 #digit #q #tab . name #q' (nat windowSize . roll #q . doubleN))
          . (pick #digit . toInt . op0 . greaterThan)
          . opIf
            ( begin
                . (roll #tab . roll #digit . toInt . i2nUnsafe)
                . (V.lookup . fromJust . roll #q' . EC.ecAddJ)
            )
            (del #tab . del #digit . roll #q')
      )

    fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
    fromJust = lambda0 (errCanNotHappen) . swap . fromMaybe'

doubleN :: Fn (s :> TNat :> TPointJ) (s :> TPointJ)
doubleN =
  opUntil
    ( begin
        . (swap . dup . op0 . equal)
        . opIf (swap . opTrue) (nat1SubUnsafe . swap . EC.ecDoubleJ . opFalse)
    )
    . nip

digitsM :: Natural -> Env (s :> TNat) (s :> V.TVector TInt8)
digitsM windowSize = lambda1 f . swap . V.unfoldr
  where
    f :: Fn (s :> TNat) (s :> TMaybe (TTuple TInt8 TNat))
    f = dup . ifZero (drop . nothing) (tup . just)

    tup :: Fn (s :> TNat) (s :> TTuple TInt8 TNat)
    tup =
      begin
        . (dup . nat numValues . mod . n2i . fromInt)
        . (swap . nat numValues . div . tuple)

    numValues = 2 ^ windowSize
