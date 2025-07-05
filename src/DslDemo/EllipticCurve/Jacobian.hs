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
import DslDemo.EllipticCurve.Field
  ( TPrimeModulus,
    feCube',
    feInv',
    feMul',
    feSquare',
    primeModulus,
  )
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

type LoopTypeN s =
  s
    > N "n" TNat
    > N "p" TPointJ
    > N "r" TPointJ
    > N "pmod" TPrimeModulus

type LoopType s = s > TNat > TPointJ > TPointJ > TPrimeModulus

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd =
  function
    ( begin
        # toJacobian
        # opSwap
        # toJacobian
        # primeModulus
        # EC.ecAdd
        # primeModulus
        # fromJacobian
    )

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble =
  function
    ( begin
        # toJacobian
        # primeModulus
        # EC.ecDouble
        # primeModulus
        # fromJacobian
    )

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul =
  function
    ( begin
        # toJacobian
        # ecMulJ
        # primeModulus
        # fromJacobian
    )

ecMulJ :: FN (s > TNat > TPointJ) (s > TPointJ)
ecMulJ = function (unname @2 ecMulJ')
  where
    ecMulJ' :: FN (s > N "n" TNat > N "p" TPointJ) (s > TPointJ)
    ecMulJ' =
      begin
        # argPick @"n"
        # (nat 0 # opNumEqual)
        # opIf
          (argDrop @"n" # argDrop @"p" # makeIdentity)
          ( begin
              # argRoll @"n"
              # argRoll @"p"
              # makeIdentity
              # primeModulus
              # opUntil (unname @4 loop)
              # opDrop
              # opNip
              # opNip
          )

    loop :: FN (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # name @"r2"
          ( begin
              # argRoll @"r"
              # ex1 (argPick @"n" # isOdd)
              # opWhen (argPick @"p" # argPick @"pmod" # EC.ecAdd)
          )
        # (argPick @"n" # half)
        # (argRoll @"p" # argPick @"pmod" # EC.ecDouble)
        # (argRoll @"r2")
        # argRoll @"pmod"
        # (argRoll @"n" # half # isZero)

toJacobian :: FN (s > TPoint) (s > TPointJ)
toJacobian = function (unname @1 toJacobian')
  where
    toJacobian' :: FN (s > N "p" TPoint) (s > TPointJ)
    toJacobian' =
      begin
        # ex1 (argPick @"p" # AP.isIdentity)
        # opIf
          (argDrop @"p" # makeIdentity)
          ( begin
              # ex1 (argPick @"p" # AP.getX)
              # (argRoll @"p" # AP.getY)
              # int 1
              # makePoint
          )

fromJacobian :: FN (s > TPointJ > TPrimeModulus) (s > TPoint)
fromJacobian = function (unname @2 fromJacobian')
  where
    fromJacobian' ::
      FN
        (s > N "p" TPointJ > N "pmod" TPrimeModulus)
        (s > TPoint)
    fromJacobian' =
      begin
        # (argPick @"p" # isIdentity)
        # opIf
          (argDrop @"pmod" # argDrop @"p" # AP.makeIdentity)
          ( begin
              # name @"z" (argPick @"p" # JP.getZ)
              # name @"x'"
                ( begin
                    # (argPick @"p" # JP.getX)
                    # argPick @"z"
                    # argPick @"pmod"
                    # feSquare'
                    # argPick @"pmod"
                    # feInv'
                    # argPick @"pmod"
                    # feMul'
                )
              # name @"y'"
                ( begin
                    # (argRoll @"p" # JP.getY)
                    # argRoll @"z"
                    # argPick @"pmod"
                    # feCube'
                    # argPick @"pmod"
                    # feInv'
                    # argRoll @"pmod"
                    # feMul'
                )
              # argRoll @"x'"
              # argRoll @"y'"
              # AP.makePoint
          )
