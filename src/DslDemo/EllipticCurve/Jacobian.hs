-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Jacobian
  ( ecDouble,
    ecAdd,
    ecMul,
    toJacobian,
    fromJacobian,
  )
where

import Alba.Dsl.V1.Bch2026.Contract.Prelude (halve, isOdd, isZero)
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

type LoopTypeN s = s > N "n" TNat > N "p" TPointJ > N "r" TPointJ

type LoopType s = s > TNat > TPointJ > TPointJ

ecAdd :: Fn (s > TPoint > TPoint) (s > TPoint)
ecAdd = fn (toJacobian # opSwap # toJacobian # EC.ecAddJ # fromJacobian)

ecDouble :: Fn (s > TPoint) (s > TPoint)
ecDouble = fn (toJacobian # EC.ecDoubleJ # fromJacobian)

ecMul :: Fn (s > TNat > TPoint) (s > TPoint)
ecMul = fn (toJacobian # ecMulJ # fromJacobian)

ecMulJ :: Fn (s > TNat > TPointJ) (s > TPointJ)
ecMulJ = unname 2 ecMulJ'
  where
    ecMulJ' :: Fn (s > N "n" TNat > N "p" TPointJ) (s > TPointJ)
    ecMulJ' =
      begin
        # pick "n"
        # (nat 0 # opNumEqual)
        # opIf
          (del "n" # del "p" # makeIdentity)
          ( begin
              # roll "n"
              # roll "p"
              # makeIdentity
              # opUntil (unname 3 loop)
              # opNip
              # opNip
          )

    loop :: Fn (LoopTypeN s) (LoopType s > TBool)
    loop =
      begin
        # name
          "r2"
          (roll "r" # pick "n" # isOdd # opWhen (pick "p" # EC.ecAddJ))
        # (pick "n" # halve)
        # (roll "p" # EC.ecDoubleJ)
        # roll "r2"
        # (roll "n" # halve # isZero)

toJacobian :: Fn (s > TPoint) (s > TPointJ)
toJacobian = fn (unname 1 toJacobian')
  where
    toJacobian' :: Fn (s > N "p" TPoint) (s > TPointJ)
    toJacobian' =
      begin
        # ex1 (pick "p" # AP.isIdentity)
        # opIf
          (del "p" # makeIdentity)
          (pick "p" # AP.getX # roll "p" # AP.getY # int 1 # makePoint)

fromJacobian :: Fn (s > TPointJ) (s > TPoint)
fromJacobian = fn (unname 1 fromJacobian')
  where
    fromJacobian' :: Fn (s > N "p" TPointJ) (s > TPoint)
    fromJacobian' =
      begin
        # (pick "p" # isIdentity)
        # opIf
          (del "p" # AP.makeIdentity)
          ( begin
              # name "z" (pick "p" # JP.getZ)
              # name
                "x'"
                (pick "p" # JP.getX # pick "z" # feSquare # feInv # feMul)
              # name
                "y'"
                (roll "p" # JP.getY # roll "z" # feCube # feInv # feMul)
              # (roll "x'" # roll "y'" # AP.makePoint)
          )
