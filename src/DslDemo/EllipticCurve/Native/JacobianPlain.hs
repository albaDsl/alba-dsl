-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.JacobianPlain (ecMul) where

import DslDemo.EllipticCurve.Native.Jacobian (PointJ (..), ecAdd, ecDouble)
import Numeric.Natural (Natural)

ecMul :: Natural -> PointJ -> PointJ
ecMul _ PJIdentity = PJIdentity
ecMul n p = ecMul' n p PJIdentity

ecMul' :: Natural -> PointJ -> PointJ -> PointJ
ecMul' 0 _ r = r
ecMul' n p r =
  let r' = if odd n then ecAdd r p else r
      p' = ecDouble p
   in ecMul' (n `div` 2) p' r'
