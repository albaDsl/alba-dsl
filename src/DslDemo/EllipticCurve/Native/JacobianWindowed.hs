-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.JacobianWindowed (ecMul) where

import Data.Vector qualified as V
import DslDemo.EllipticCurve.Native.Jacobian (PointJ (..), ecAdd, ecDoubleN)
import Numeric.Natural (Natural)

windowSize :: Natural
windowSize = 4

ecMul :: Natural -> PointJ -> PointJ
ecMul _ PJIdentity = PJIdentity
ecMul n p = ecMul' table (digits n []) PJIdentity
  where
    numValues :: Natural
    numValues = 2 ^ windowSize

    table :: V.Vector PointJ
    table = V.iterateN (fromIntegral numValues) (ecAdd p) PJIdentity

    digits :: Natural -> [Natural] -> [Natural]
    digits 0 acc = acc
    digits x acc = digits (x `div` numValues) (x `rem` numValues : acc)

ecMul' :: V.Vector PointJ -> [Natural] -> PointJ -> PointJ
ecMul' _table [] q = q
ecMul' table (digit : rest) q =
  let q' = ecDoubleN windowSize q
      q'' =
        if digit > 0
          then ecAdd q' (table V.! fromIntegral digit)
          else q'
   in ecMul' table rest q''
