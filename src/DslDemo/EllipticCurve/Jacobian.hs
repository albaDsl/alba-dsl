-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Jacobian
  ( ecDouble,
    ecAdd,
    ecMul,
    toJacobian,
    fromJacobian,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Fn,
    TNat,
    begin,
    del,
    int,
    name,
    name3,
    nat,
    ns,
    ns2,
    ns3,
    opIf,
    opWhen,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (equal),
    halve,
    isOdd,
    isZero,
    nip,
    swap,
  )
import Alba.Dsl.V1.Bch2026.Lang (fn)
import Alba.Dsl.V1.Bch2026.LangArgs (Loop)
import Alba.Dsl.V1.Bch2026.Ops (opUntil)
import DslDemo.EllipticCurve.Field (feCube, feInv, feMul, feSquare)
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    isIdentity,
    makeIdentity,
    makePoint,
  )
import DslDemo.EllipticCurve.JacobianPoint qualified as JP
import DslDemo.EllipticCurve.Point (TPoint)
import DslDemo.EllipticCurve.Point qualified as AP

{- ORMOLU_DISABLE -}
type N = "n"; type P = "p"; type R = "r"; type R2 = "r2"; type X = "x";
type Y = "y"; type Z = "z"
{- ORMOLU_ENABLE -}

ecAdd :: Fn (s > TPoint > TPoint) (s > TPoint)
ecAdd = fn (toJacobian # swap # toJacobian # EC.ecAddJ # fromJacobian)

ecDouble :: Fn (s > TPoint) (s > TPoint)
ecDouble = fn (toJacobian # EC.ecDoubleJ # fromJacobian)

ecMul :: Fn (s > TNat > TPoint) (s > TPoint)
ecMul = fn (toJacobian # ecMulJ # fromJacobian)

ecMulJ :: Fn (s > TNat > TPointJ) (s > TPointJ)
ecMulJ =
  begin
    # (ns2 N P # pick N # nat 0 # equal)
    # opIf
      (del N # del P # makeIdentity)
      (roll N # roll P # makeIdentity # opUntil loop # nip # nip)
  where
    loop :: Loop (s > TNat > TPointJ > TPointJ)
    loop =
      begin
        # ns3 N P R
        # name R2 (roll R # pick N # isOdd # opWhen (pick P # EC.ecAddJ))
        # (pick N # halve # roll P # EC.ecDoubleJ # roll R2)
        # (roll N # halve # isZero)

toJacobian :: Fn (s > TPoint) (s > TPointJ)
toJacobian =
  fn
    ( begin
        # (ns P # pick P # AP.isIdentity)
        # opIf
          (del P # makeIdentity)
          (pick P # AP.getX # roll P # AP.getY # int 1 # makePoint)
    )

fromJacobian :: Fn (s > TPointJ) (s > TPoint)
fromJacobian =
  fn
    ( begin
        # ns P
        # (pick P # isIdentity)
        # opIf
          (del P # AP.makeIdentity)
          ( begin
              # name3 X Y Z (roll P # JP.getXYZ')
              # (roll X # pick Z # feSquare # feInv # feMul)
              # (roll Y # roll Z # feCube # feInv # feMul)
              # AP.makePoint
          )
    )
