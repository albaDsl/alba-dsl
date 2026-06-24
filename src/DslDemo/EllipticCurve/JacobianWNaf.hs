-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.JacobianWNaf
  ( TTable,
    setupTable,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    StackEntry,
    TInt,
    TNat,
    del,
    fn,
    i2nUnsafe,
    int,
    n2i,
    name,
    nat,
    ns,
    ns2,
    ns4,
    opRShiftNum,
    opWhen,
    pick,
    quot0,
    quot1,
    quot2,
    quot3,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026 qualified as Bch
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( Integral (div, toInt),
    Ord (..),
    TInt16,
    TMaybe,
    TTuple,
    abs,
    apply2,
    apply3,
    dup,
    errCanNotHappen,
    fromInt,
    fromMaybe',
    ifZero,
    just,
    nothing,
    rot,
    sub,
    sub1,
    swap,
    tuple,
    untuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances ()
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import DslDemo.EllipticCurve.Common (countTrailingZeros, doubleN, mods)
import DslDemo.EllipticCurve.Jacobian
  ( ecAdd,
    ecDouble,
    fromJacobian,
    toJacobian,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import DslDemo.EllipticCurve.Point (TPoint)
import Prelude (Int, (-), (^))

type TTable = TVector TPointJ

windowSize :: Int
windowSize = 5

setupTable :: Bch.Fn (s :> TPoint) (s :> TTable)
setupTable =
  fn
    ( (toJacobian . ns #p . nat numValues . pick #p . dup . EC.ecAddJ)
        . (quot2 EC.ecAddJ . apply2 . roll #p . V.iterateN)
    )
  where
    numValues = 2 ^ (windowSize - 1)

lookup :: Fn (s :> TTable :> TInt) (s :> TPointJ)
lookup =
  (dup . int 0 . lessThan . rot . rot . abs . i2nUnsafe . sub1 . nat 2)
    . (div . V.lookup . fromJust . swap . opWhen EC.ecNegateJ)
  where
    fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
    fromJust = quot0 errCanNotHappen . swap . fromMaybe'

ecMul :: Fn (s :> TTable :> TNat) (s :> TPoint)
ecMul =
  fn
    ( (ns2 #tab #n . roll #tab . quot3 f . apply3 . makeIdentity . roll #n)
        . (chunksM . V.foldr . fromJacobian)
    )
  where
    f :: Fn (s :> TTuple TInt16 TInt16 :> TPointJ :> TTable) (s :> TPointJ)
    f =
      (rot . untuple . ns4 #acc #tab #d #z . roll #z . toInt . i2nUnsafe)
        . (roll #acc . roll #tab . roll #d . toInt . lookup . EC.ecAddJ)
        . doubleN

chunksM :: Fn (s :> TNat) (s :> V.TVector (TTuple TInt16 TInt16))
chunksM = n2i . quot1 step . swap . V.unfoldr
  where
    step :: Fn (s :> TInt) (s :> TMaybe (TTuple (TTuple TInt16 TInt16) TInt))
    step =
      (ns #m . pick #m)
        . ifZero
          (del #m . nothing)
          ( (name #z (pick #m . countTrailingZeros))
              . (name #m') (roll #m . pick #z . opRShiftNum)
              . (name #d (pick #m' . mods windowSize))
              . (pick #d . i2Int16 . roll #z . n2i . i2Int16 . tuple)
              . (roll #m' . roll #d . sub . tuple . just)
          )

    i2Int16 :: Fn (s :> TInt) (s :> TInt16)
    i2Int16 = fromInt
