-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.JacobianWNafGlv
  ( ecMul,
    setupTable,
    lookup,
  )
where

import DslDemo.EllipticCurve.Constants qualified as C
import DslDemo.EllipticCurve.Native.Affine (Point)
import DslDemo.EllipticCurve.Native.Glv (glvDecompose, phi)
import DslDemo.EllipticCurve.Native.Jacobian (PointJ (..), ecNegate)
import DslDemo.EllipticCurve.Native.JacobianWNafInterleaved
  ( ecMulInterleaved,
    lookup,
    setupTable,
  )
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

ecMul :: Natural -> PointJ -> PointJ
ecMul k p =
  let (k1, k2) = glvDecompose (fromIntegral k `mod` C.n)
      phiP = phi p
   in ecMulInterleaved [variableBase k1 p, variableBase k2 phiP]

variableBase :: Integer -> PointJ -> (Integer, Integer -> Point)
variableBase k p =
  let base = if k >= 0 then p else ecNegate p
   in (abs k, lookup (setupTable base))

-- fixedBase :: V.Vector PointJ -> Integer -> (Integer, Integer -> Point)
-- fixedBase tab k
--   | k >= 0 = (k, lookup tab)
--   | otherwise = (-k, ecNegate . lookup tab)
