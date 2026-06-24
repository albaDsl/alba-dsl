-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.Glv (glvDecompose, phi) where

import DslDemo.EllipticCurve.Constants qualified as C
import DslDemo.EllipticCurve.Native.Jacobian (PointJ (..))
import Prelude hiding (lookup)

-- https://github.com/bitcoin-core/secp256k1/blob/master/src/scalar_impl.h
-- https://bitcointalk.org/index.php?topic=3238.0
glvDecompose :: Integer -> (Integer, Integer)
glvDecompose k =
  let c1 = roundDiv (C.glvB2 * k) C.n
      c2 = roundDiv (-C.glvB1 * k) C.n
      k1 = k - c1 * C.glvA1 - c2 * C.glvA2
      k2 = -c1 * C.glvB1 - c2 * C.glvB2
   in (k1, k2)

roundDiv :: Integer -> Integer -> Integer
roundDiv a b
  | b < 0 = roundDiv (-a) (-b)
  | otherwise = (2 * a + signum a * b) `quot` (2 * b)

phi :: PointJ -> PointJ
phi PJIdentity = PJIdentity
phi (PJ x y z) = PJ (fromIntegral C.beta * x) y z
