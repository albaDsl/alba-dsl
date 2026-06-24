-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Jacobian
  ( ecDouble,
    ecAdd,
    ecMul,
    toJacobian,
    fromJacobian,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    Stack (..),
    TNat,
    begin,
    del,
    fn,
    name,
    nat,
    ns2,
    ns3,
    opIf,
    opUntil,
    opWhen,
    pick,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (equal),
    halve,
    isOdd,
    isZero,
    nip,
    swap,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    fromJacobian,
    makeIdentity,
    toJacobian,
  )
import DslDemo.EllipticCurve.Point (TPoint)
import Prelude ()

ecAdd :: Fn (s :> TPoint :> TPoint) (s :> TPoint)
ecAdd = fn (toJacobian . swap . toJacobian . EC.ecAddJ . fromJacobian)

ecDouble :: Fn (s :> TPoint) (s :> TPoint)
ecDouble = fn (toJacobian . EC.ecDoubleJ . fromJacobian)

ecMul :: Fn (s :> TNat :> TPoint) (s :> TPoint)
ecMul = fn (toJacobian . ecMulJ . fromJacobian)

ecMulJ :: Fn (s :> TNat :> TPointJ) (s :> TPointJ)
ecMulJ =
  begin
    . (ns2 #n #p . pick #n . nat 0 . equal)
    . opIf
      (del #n . del #p . makeIdentity)
      (roll #n . roll #p . makeIdentity . opUntil loop . nip . nip)
  where
    loop :: Loop (s :> TNat :> TPointJ :> TPointJ)
    loop =
      begin
        . ns3 #n #p #r
        . name #r2 (roll #r . pick #n . isOdd . opWhen (pick #p . EC.ecAddJ))
        . (pick #n . halve . roll #p . EC.ecDoubleJ . roll #r2)
        . (roll #n . halve . isZero)
