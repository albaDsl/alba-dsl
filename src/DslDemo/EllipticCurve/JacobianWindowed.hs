-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianWindowed
  ( setupTable,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Fn,
    TBytes,
    TNat,
    begin,
    cast,
    del,
    i2nUnsafe,
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
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2025.Contract.Prelude (nat1SubUnsafe)
import Alba.Dsl.V1.Bch2026 (Env, fn, lambda1, lambda3, opUntil)
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    Integral (..),
    Ord (..),
    TInt8,
    TTuple,
    apply3,
    drop,
    dup,
    ifZero,
    just,
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
import Prelude hiding (div, drop, mod)

type TFId = TNat -- Function Id

type TTab = TFId -- Lookup table (represented by the base Function Id)

{- ORMOLU_DISABLE -}
type Acc = "acc"; type Digit = "digit"; type FId = "FId"; type I = "i";
type N = "n"; type P = "m"; type Q = "q"; type Q' = "q'"; type Tab = "tab";
{- ORMOLU_ENABLE -}

setupTable :: Fn (s > TFId > TPoint) s
setupTable = toJacobian # makeIdentity # nat numValues # setupTable'
  where
    setupTable' =
      fn
        ( begin
            # (ns4 FId P Acc I # pick I # op0 # equal)
            # opIf
              (del FId # del P # del Acc # del I)
              ( begin
                  # (pick Acc # p2b # pick FId # defineConstant)
                  # (roll FId # add1)
                  # (pick P # roll Acc # roll P # EC.ecAddJ)
                  # (roll I # nat1SubUnsafe # setupTable')
              )
        )

    p2b :: Fn (s > TPointJ) (s > TBytes)
    p2b = cast

ecMul :: Env (s > TTab > TNat) (s > TPoint)
ecMul =
  fn
    ( begin
        # (ns2 Tab N # pick N # nat 0 # equal)
        # opIf
          (del Tab # del N # makeIdentity)
          ( begin
              # (roll Tab # lambda3 f # apply3)
              # (makeIdentity # roll N # digits # V.foldr)
          )
        # fromJacobian
    )
  where
    f :: Fn (s > TInt8 > TPointJ > TTab) (s > TPointJ)
    f =
      ( begin
          # ns3 Digit Q Tab
          # name Q' (nat windowSize # roll Q # doubleN)
          # (pick Digit # toInt # op0 # greaterThan)
          # opIf
            ( begin
                # (roll Tab # roll Digit # toInt # i2nUnsafe)
                # (tableLookup # roll Q' # EC.ecAddJ)
            )
            (del Tab # del Digit # roll Q')
      )

    tableLookup :: Fn (s > TTab > TNat) (s > TPointJ)
    tableLookup = add # getConstant # b2p

    b2p :: Fn (s > TBytes) (s > TPointJ)
    b2p = cast

doubleN :: Fn (s > TNat > TPointJ) (s > TPointJ)
doubleN =
  opUntil
    ( begin
        # (swap # dup # op0 # equal)
        # opIf
          (swap # opTrue)
          (nat1SubUnsafe # swap # EC.ecDoubleJ # opFalse)
    )
    # nip

digits :: Fn (s > TNat) (s > V.TVector TInt8)
digits =
  lambda1 (dup # ifZero (drop # nothing) (tup # just)) # swap # V.unfoldr
  where
    tup :: Fn (s > TNat) (s > TTuple TInt8 TNat)
    tup = dup # wmod # swap # wdiv # tuple

    wmod = nat numValues # mod # n2i # fromInt
    wdiv = nat numValues # div

windowSize :: Natural
windowSize = 4

numValues :: Natural
numValues = 2 ^ windowSize
