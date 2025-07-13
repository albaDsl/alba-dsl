-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Jacobian
  ( ecDouble,
    ecAdd,
    ecMul,
    toJacobian,
    fromJacobian,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Math (half, isOdd)
import Alba.Dsl.V1.Bch2026
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
import Prelude hiding (drop)

type LoopTypeN s = s > N "n" TNat > N "p" TPointJ > N "r" TPointJ

type LoopType s = s > TNat > TPointJ > TPointJ

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd = function (toJacobian # opSwap # toJacobian # EC.ecAddJ # fromJacobian)

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = function (toJacobian # EC.ecDoubleJ # fromJacobian)

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = function (toJacobian # ecMulJ # fromJacobian)

ecMulJ :: FN (s > TNat > TPointJ) (s > TPointJ)
ecMulJ = function (unname @2 ecMulJ')
  where
    ecMulJ' :: FN (s > N "n" TNat > N "p" TPointJ) (s > TPointJ)
    ecMulJ' =
      begin
        # pick @"n"
        # (nat 0 # opNumEqual)
        # opIf
          (drop @"n" # drop @"p" # makeIdentity)
          ( begin
              # roll @"n"
              # roll @"p"
              # makeIdentity
              # opUntil (unname @3 loop)
              # opNip
              # opNip
          )

    loop :: FN (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # name @"r2"
          ( begin
              # roll @"r"
              # ex1 (pick @"n" # isOdd)
              # opWhen (pick @"p" # EC.ecAddJ)
          )
        # (pick @"n" # half)
        # (roll @"p" # EC.ecDoubleJ)
        # (roll @"r2")
        # (roll @"n" # half # isZero)

toJacobian :: FN (s > TPoint) (s > TPointJ)
toJacobian = function (unname @1 toJacobian')
  where
    toJacobian' :: FN (s > N "p" TPoint) (s > TPointJ)
    toJacobian' =
      begin
        # ex1 (pick @"p" # AP.isIdentity)
        # opIf
          (drop @"p" # makeIdentity)
          ( begin
              # ex1 (pick @"p" # AP.getX)
              # (roll @"p" # AP.getY)
              # int 1
              # makePoint
          )

fromJacobian :: FN (s > TPointJ) (s > TPoint)
fromJacobian = function (unname @1 fromJacobian')
  where
    fromJacobian' :: FN (s > N "p" TPointJ) (s > TPoint)
    fromJacobian' =
      begin
        # (pick @"p" # isIdentity)
        # opIf
          (drop @"p" # AP.makeIdentity)
          ( begin
              # name @"z" (pick @"p" # JP.getZ)
              # name @"x'"
                ( begin
                    # (pick @"p" # JP.getX)
                    # (pick @"z" # feSquare # feInv)
                    # feMul
                )
              # name @"y'"
                ( begin
                    # (roll @"p" # JP.getY)
                    # (roll @"z" # feCube # feInv)
                    # feMul
                )
              # (roll @"x'" # roll @"y'" # AP.makePoint)
          )
