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
    (.),
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
import Prelude ()

ecAdd :: Fn (s > TPoint > TPoint) (s > TPoint)
ecAdd = fn (toJacobian . swap . toJacobian . EC.ecAddJ . fromJacobian)

ecDouble :: Fn (s > TPoint) (s > TPoint)
ecDouble = fn (toJacobian . EC.ecDoubleJ . fromJacobian)

ecMul :: Fn (s > TNat > TPoint) (s > TPoint)
ecMul = fn (toJacobian . ecMulJ . fromJacobian)

ecMulJ :: Fn (s > TNat > TPointJ) (s > TPointJ)
ecMulJ =
  begin
    . (ns2 #n #p . pick #n . nat 0 . equal)
    . opIf
      (del #n . del #p . makeIdentity)
      (roll #n . roll #p . makeIdentity . opUntil loop . nip . nip)
  where
    loop :: Loop (s > TNat > TPointJ > TPointJ)
    loop =
      begin
        . ns3 #n #p #r
        . name #r2 (roll #r . pick #n . isOdd . opWhen (pick #p . EC.ecAddJ))
        . (pick #n . halve . roll #p . EC.ecDoubleJ . roll #r2)
        . (roll #n . halve . isZero)

toJacobian :: Fn (s > TPoint) (s > TPointJ)
toJacobian =
  fn
    ( begin
        . (ns #p . pick #p . AP.isIdentity)
        . opIf
          (del #p . makeIdentity)
          (roll #p . AP.getXY' . int 1 . makePoint)
    )

fromJacobian :: Fn (s > TPointJ) (s > TPoint)
fromJacobian =
  fn
    ( begin
        . ns #p
        . (pick #p . isIdentity)
        . opIf
          (del #p . AP.makeIdentity)
          ( begin
              . name3 #x #y #z (roll #p . JP.getXYZ')
              . (roll #x . pick #z . feSquare . feInv . feMul)
              . (roll #y . roll #z . feCube . feInv . feMul)
              . AP.makePoint
          )
    )
