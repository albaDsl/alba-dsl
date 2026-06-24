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
    name2,
    nat,
    ns,
    ns2,
    ns3,
    op0,
    opFalse,
    opIf,
    opRShiftNum,
    opTrue,
    opUntil,
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
  ( BlobEq (equal),
    Integral (add1, div, mod, toInt),
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
    isEven,
    just,
    mod,
    nip,
    nothing,
    over,
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
    numValues = 2 ^ (windowSize - 1) - 1

ecMul :: Fn (s :> TTable :> TNat) (s :> TPoint)
ecMul =
  fn
    ( (ns2 #tab #n . pick #n . nat 0 . equal)
        . (opIf (del #tab . del #n . makeIdentity))
          ( name2 #chunks #z (roll #n . chunksM . untuple)
              . (roll #tab . quot3 f . apply3)
              . (makeIdentity . roll #chunks . V.foldr)
              . (roll #z . swap . doubleN)
          )
        . fromJacobian
    )
  where
    f :: Fn (s :> TTuple TInt16 TInt16 :> TPointJ :> TTable) (s :> TPointJ)
    f =
      ( (ns3 #chunk #q #tab . name2 #d #z (roll #chunk . untuple))
          . (roll #z . toInt . i2nUnsafe . roll #q . doubleN)
          . (roll #tab . roll #d . toInt . lookup . EC.ecAddJ)
      )

    lookup :: Fn (s :> TTable :> TInt) (s :> TPointJ)
    lookup =
      (dup . int 0 . lessThan . rot . rot . abs . i2nUnsafe . sub1 . nat 2)
        . (div . V.lookup . fromJust . swap . opWhen EC.ecNegateJ)

    fromJust :: forall a s. (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
    fromJust = quot0 errCanNotHappen . swap . fromMaybe'

chunksM :: Fn (s :> TNat) (s :> TTuple (V.TVector (TTuple TInt16 TInt16)) TNat)
chunksM =
  (n2i . ns #n)
    . (name #z0 (pick #n . countTrailingZeros))
    . (name #n' (roll #n . pick #z0 . opRShiftNum))
    . (quot1 step . roll #n' . V.unfoldr . roll #z0 . tuple)
  where
    step :: Fn (s :> TInt) (s :> TMaybe (TTuple (TTuple TInt16 TInt16) TInt))
    step =
      (ns #n . pick #n)
        . ifZero
          (del #n . nothing)
          ( (name #d (pick #n . mods))
              . (name #n') (roll #n . pick #d . sub . nat 1 . opRShiftNum)
              . (name #z (pick #n' . countTrailingZeros))
              . (name #n'' (roll #n' . pick #z . opRShiftNum))
              . ( (name #gap)
                    ( pick #n''
                        . ifZero (del #z . int 0) (roll #z . add1 . n2i)
                    )
                )
              . (roll #d . i2Int16 . roll #gap . i2Int16 . tuple)
              . (roll #n'' . tuple . just)
          )

    i2Int16 :: Fn (s :> TInt) (s :> TInt16)
    i2Int16 = fromInt

    mods :: Fn (s :> TInt) (s :> TInt)
    mods =
      (full . mod . ns #r . pick #r . half . greaterThanOrEqual)
        . (opIf (roll #r . full . sub) (roll #r))

    full = int (2 ^ windowSize)
    half = int (2 ^ (windowSize - 1))

    countTrailingZeros :: Fn (s :> TInt) (s :> TNat)
    countTrailingZeros =
      dup
        . (ifZero i2nUnsafe)
          ( nat 0
              . opUntil
                ( (over . isEven)
                    . opIf
                      (add1 . swap . nat 1 . opRShiftNum . swap . opFalse)
                      opTrue
                )
              . nip
          )

doubleN :: Fn (s :> TNat :> TPointJ) (s :> TPointJ)
doubleN =
  fn
    ( opUntil
        ( (swap . dup . op0 . equal)
            . opIf (swap . opTrue) (sub1 . swap . EC.ecDoubleJ . opFalse)
        )
        . nip
    )
