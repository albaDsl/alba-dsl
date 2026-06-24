-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.JacobianWNafGlv
  ( ecMul,
    setupTable,
    lookup,
    variableBase,
    fixedBase,
  )
where

import DslDemo.EllipticCurve.Constants qualified as C
import DslDemo.EllipticCurve.Native.Affine (Point)
import DslDemo.EllipticCurve.Native.Affine qualified as AP
import DslDemo.EllipticCurve.Native.Glv (glvDecompose)
import DslDemo.EllipticCurve.Native.Jacobian (PointJ (..), ecNegate)
import DslDemo.EllipticCurve.Native.JacobianWNafInterleaved
  ( Table,
    ecMulInterleaved,
    lookup,
    setupTable,
  )
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

ecMul :: (Table, Table) -> Natural -> PointJ
ecMul (tab, tabPhi) k =
  let (k1, k2) = glvDecompose (fromIntegral k `mod` C.n)
   in ecMulInterleaved [fixedBase tab k1, fixedBase tabPhi k2]

variableBase :: Integer -> PointJ -> (Integer, Integer -> Point)
variableBase k p =
  let base = if k >= 0 then p else ecNegate p
   in (abs k, lookup (setupTable base))

fixedBase :: Table -> Integer -> (Integer, Integer -> Point)
fixedBase tab k
  | k >= 0 = (k, lookup tab)
  | otherwise = (-k, AP.ecNegate . lookup tab)
