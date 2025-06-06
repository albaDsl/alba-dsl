module DslDemo.EllipticCurve.EllipticCurve
  ( ecAdd,
    ecDouble,
    ecMul,
    ecAddP,
    ecDoubleP,
    ecMulP,
  )
where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.EllipticCurveField (primeModulus)
import DslDemo.EllipticCurve.EllipticCurvePackedNonRecursive qualified as EP
import DslDemo.EllipticCurve.EllipticCurvePoint (TPoint, packPoint, unpackPoint)
import DslDemo.EllipticCurve.EllipticCurveUnpackedNonRecursive qualified as E

ecAdd :: FN (s > TPoint > TPoint) (s > TPoint)
ecAdd =
  begin
    # unpackPoint
    # opRoll @3
    # unpackPoint
    # primeModulus
    # E.ecAdd
    # packPoint

ecDouble :: FN (s > TPoint) (s > TPoint)
ecDouble = unpackPoint # primeModulus # E.ecDouble # packPoint

ecMul :: FN (s > TNat > TPoint) (s > TPoint)
ecMul = unpackPoint # E.ecMul # packPoint

ecAddP :: FN (s > TPoint > TPoint) (s > TPoint)
ecAddP = primeModulus # EP.ecAdd

ecDoubleP :: FN (s > TPoint) (s > TPoint)
ecDoubleP = primeModulus # EP.ecDouble

ecMulP :: FN (s > TNat > TPoint) (s > TPoint)
ecMulP = EP.ecMul
