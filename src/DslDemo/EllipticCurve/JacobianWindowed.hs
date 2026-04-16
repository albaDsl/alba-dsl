-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianWindowed
  ( setupTable,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Env,
    Fn,
    Stack (..),
    TBytes,
    TFunctionId,
    TNat,
    begin,
    cast,
    del,
    fn,
    functionId,
    i2nUnsafe,
    lambda1,
    lambda3,
    n2i,
    name,
    nat,
    ns2,
    ns3,
    ns4,
    op0,
    opFalse,
    opIf,
    opTrue,
    opUntil,
    pick,
    reserveSlots,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    Integral (..),
    Ord (..),
    TInt8,
    TTuple,
    apply3,
    drop,
    dup,
    functionIdOffset,
    ifZero,
    just,
    nat1SubUnsafe,
    nip,
    nothing,
    swap,
    tuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import DslDemo.EllipticCurve.Jacobian
  ( ecAdd,
    ecDouble,
    fromJacobian,
    toJacobian,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import DslDemo.EllipticCurve.LookupTable (defineConstant, getConstant)
import DslDemo.EllipticCurve.Point (TPoint)
import Numeric.Natural (Natural)
import Prelude (Int, fromIntegral, (+), (-), (^))

type TTab = TFunctionId -- Lookup table (represented by the base Function Id)

setupTable :: Int -> Fn (s :> TPoint) s
setupTable tableStart =
  begin
    . reserveSlots [tableStart .. tableStart + fromIntegral numValues - 1]
    . (functionId (fromIntegral tableStart) . swap)
    . (toJacobian . makeIdentity . nat numValues . setupTable')
  where
    setupTable' =
      fn
        ( begin
            . (ns4 #fId #p #acc #i . pick #i . op0 . equal)
            . opIf
              (del #fId . del #p . del #acc . del #i)
              ( begin
                  . (pick #acc . p2b . pick #fId . defineConstant)
                  . (roll #fId . nat 1 . functionIdOffset)
                  . (pick #p . roll #acc . roll #p . EC.ecAddJ)
                  . (roll #i . nat1SubUnsafe . setupTable')
              )
        )

    p2b :: Fn (s :> TPointJ) (s :> TBytes)
    p2b = cast

ecMul :: Env (s :> TTab :> TNat) (s :> TPoint)
ecMul =
  fn
    ( begin
        . (ns2 #tab #n . pick #n . nat 0 . equal)
        . opIf
          (del #tab . del #n . makeIdentity)
          ( begin
              . (roll #tab . lambda3 f . apply3)
              . (makeIdentity . roll #n . digits . V.foldr)
          )
        . fromJacobian
    )
  where
    f :: Fn (s :> TInt8 :> TPointJ :> TTab) (s :> TPointJ)
    f =
      ( begin
          . ns3 #digit #q #tab
          . name #q' (nat windowSize . roll #q . doubleN)
          . (pick #digit . toInt . op0 . greaterThan)
          . opIf
            ( begin
                . (roll #tab . roll #digit . toInt . i2nUnsafe)
                . (tableLookup . roll #q' . EC.ecAddJ)
            )
            (del #tab . del #digit . roll #q')
      )

    tableLookup :: Fn (s :> TTab :> TNat) (s :> TPointJ)
    tableLookup = functionIdOffset . getConstant . b2p

    b2p :: Fn (s :> TBytes) (s :> TPointJ)
    b2p = cast

doubleN :: Fn (s :> TNat :> TPointJ) (s :> TPointJ)
doubleN =
  opUntil
    ( begin
        . (swap . dup . op0 . equal)
        . opIf
          (swap . opTrue)
          (nat1SubUnsafe . swap . EC.ecDoubleJ . opFalse)
    )
    . nip

digits :: Fn (s :> TNat) (s :> V.TVector TInt8)
digits =
  lambda1 (dup . ifZero (drop . nothing) (tup . just)) . swap . V.unfoldr
  where
    tup :: Fn (s :> TNat) (s :> TTuple TInt8 TNat)
    tup = dup . wmod . swap . wdiv . tuple

    wmod = nat numValues . mod . n2i . fromInt
    wdiv = nat numValues . div

windowSize :: Natural
windowSize = 4

numValues :: Natural
numValues = 2 ^ windowSize
