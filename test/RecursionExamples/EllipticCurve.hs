{-# OPTIONS_GHC -w #-}

module RecursionExamples.EllipticCurve
  ( ecDouble,
    ecAdd,
    ecMul,
    ecDoubleP,
    ecAddP,
    ecMulP,
  )
where

import Alba.Dsl.V1.Bch2026
import RecursionExamples.EllipticCurvePacked qualified as EP
import RecursionExamples.EllipticCurvePoint (TPoint, packPoint, unpackPoint)
import RecursionExamples.EllipticCurveUnpacked qualified as EUP

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = unpackPoint # EUP.ecDouble # packPoint

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd = opSwap # unpackPoint # opRoll @3 # unpackPoint # EUP.ecAdd # packPoint

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = unpackPoint # EUP.ecMul # packPoint

ecDoubleP :: FN (s > TPoint) (s > TPoint)
ecDoubleP = EP.ecDouble

ecAddP :: FN (s > TPoint > TPoint) (s > TPoint)
ecAddP = EP.ecAdd

ecMulP :: FN (s > TNat > TPoint) (s > TPoint)
ecMulP = EP.ecMul
