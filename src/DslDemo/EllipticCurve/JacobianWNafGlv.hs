-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module DslDemo.EllipticCurve.JacobianWNafGlv
  ( TTable,
    setupTable,
    ecMul,
    ecDouble,
    ecAdd,
    phi,
    phi',
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026 qualified as Bch
import Alba.Dsl.V1.Bch2026.Contract.Math (signum)
import Alba.Dsl.V1.Bch2026.Contract.PartialApplicationB qualified as QB
import Alba.Dsl.V1.Bch2026.Contract.Prelude
import Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances ()
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.Contract.VectorAlgorithms (countingSortDesc)
import Alba.Dsl.V1.Bch2026.QuotationsB qualified as QB
import DslDemo.EllipticCurve.Common (countTrailingZeros, doubleN, mods)
import DslDemo.EllipticCurve.Constants qualified as C
import DslDemo.EllipticCurve.Field (feMul, pushFe)
import DslDemo.EllipticCurve.Jacobian
  ( ecAdd,
    ecDouble,
    fromJacobian,
    toJacobian,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ)
import DslDemo.EllipticCurve.JacobianPoint qualified as JP
import DslDemo.EllipticCurve.JacobianWNafInterleaved (ecMulInterleaved)
import DslDemo.EllipticCurve.Point (TPoint)
import DslDemo.EllipticCurve.Point qualified as AP
import Prelude (Int, fromIntegral, id, undefined, (-), (^))

type TTable = TVector TPointJ

type TScalarAndLookup = TTuple TInt264 (TQuotB '[TInt16] '[TPointJ])

type TTerm = TTuple TInt16 TInt16

type TTerm' = TTuple (TTuple TInt16 TInt16) (TQuotB '[TInt16] '[TPointJ])

windowSize :: Int
windowSize = 5

setupTable :: Bch.Fn (s :> TPoint) (s :> TTable)
setupTable =
  fn
    ( (toJacobian . ns #p . nat numValues . pick #p . EC.ecDoubleJ)
        . (quot2 EC.ecAddJ . apply2 . roll #p . V.iterateN)
    )
  where
    numValues = 2 ^ (windowSize - 1)

lookup :: Fn (s :> TTable :> TInt) (s :> TPointJ)
lookup =
  (dup . int 0 . lessThan . rot . rot . abs . i2nUnsafe . sub1 . nat 2)
    . (div . V.lookup . fromJust . swap . opWhen EC.ecNegateJ)

ecMul :: Env (s :> TTuple TTable TTable :> TNat) (s :> TPoint)
ecMul =
  (swap . untuple . ns3 #n #tabP #tabPhiP)
    . (roll #n . n2i . glvDecompose)
    . (roll #tabPhiP . swap . fixedBase . swap)
    . (roll #tabP . swap . fixedBase)
    . (V.empty . V.cons . V.cons . ecMulInterleaved . fromJacobian)
  where
    fixedBase ::
      Env
        (s :> TTable :> TInt)
        (s :> TTuple TInt264 (TQuotB '[TInt16] '[TPointJ]))
    fixedBase =
      fn
        ( (dup . int 0 . greaterThanOrEqual)
            . opIf
              (i2Int264 . swap . QB.quot2 lookup' . QB.apply2 . tuple)
              ( (i2Int264 . negate . swap . QB.quot2 lookupNeg . QB.apply2)
                  . tuple
              )
        )

    i2Int264 :: Fn (s :> TInt) (s :> TInt264)
    i2Int264 = fromInt

    lookup' :: Fn (s :> TInt16 :> TTable) (s :> TPointJ)
    lookup' = swap . toInt . lookup

    lookupNeg :: Fn (s :> TInt16 :> TTable) (s :> TPointJ)
    lookupNeg = lookup' . EC.ecNegateJ

fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
fromJust = quot0 errCanNotHappen . swap . fromMaybe'

glvDecompose :: Fn (s :> TInt) (s :> TInt :> TInt)
glvDecompose =
  ns #k
    . name #c1 (int C.glvB2 . pick #k . mul . int C.n . roundDiv)
    . name #c2 (int C.glvB1 . negate . pick #k . mul . int C.n . roundDiv)
    . (name #k1)
      ( (roll #k . pick #c1 . int C.glvA1 . mul . sub . pick #c2)
          . (int C.glvA2 . mul . sub)
      )
    . (name #k2)
      ( (roll #c1 . negate . int C.glvB1 . mul . int C.glvB2)
          . (roll #c2 . mul . sub)
      )
    . (roll #k1 . roll #k2)

roundDiv :: Fn (s :> TInt :> TInt) (s :> TInt)
roundDiv =
  fn
    ( ns2 #a #b
        . (pick #b . int 0 . lessThan)
        . opIf
          (roll #a . negate . roll #b . negate . roundDiv)
          ( (pick #a . int 2 . mul . roll #a . signum . pick #b . mul . add)
              . (roll #b . int 2 . mul . div)
          )
    )

phi :: Fn (s :> TPointJ) (s :> TPointJ)
phi =
  (dup . JP.isIdentity)
    . opIf
      id
      ( (JP.getXYZ . rot . pushFe (fromIntegral C.beta) . feMul)
          . (rot . rot . JP.makePoint)
      )

phi' :: Fn (s :> TPoint) (s :> TPoint)
phi' =
  (dup . AP.isIdentity)
    . opIf
      id
      ( (AP.getXY . swap . pushFe (fromIntegral C.beta) . feMul)
          . (swap . AP.makePoint)
      )
