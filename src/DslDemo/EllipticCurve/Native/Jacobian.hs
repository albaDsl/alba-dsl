-- Copyright (c) 2025 albaDsl

-- Implementation modeled after: "Fast Prime Field Elliptic Curve Cryptography
-- with 256 Bit Primes", Shay Gueron, Vlad Krasnov.

module DslDemo.EllipticCurve.Native.Jacobian
  ( Point (..),
    PointJ (..),
    FieldElement (..),
    g,
    ecAdd,
    ecAddMixed,
    ecDouble,
    ecDoubleN,
    ecNegate,
    fromJacobian,
  )
where

import DslDemo.EllipticCurve.Native.Affine (Point (..))
import DslDemo.EllipticCurve.Native.FieldElement (FieldElement (..))
import Numeric.Natural (Natural)

data PointJ = PJ !FieldElement !FieldElement !FieldElement | PJIdentity
  deriving (Eq, Show)

g :: PointJ
g =
  PJ
    0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
    0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    1

ecAdd :: PointJ -> PointJ -> PointJ
ecAdd PJIdentity p = p
ecAdd p PJIdentity = p
ecAdd p1@(PJ x1 y1 z1) (PJ x2 y2 z2) =
  let u1 = x1 * z2 ^ (2 :: Int)
      u2 = x2 * z1 ^ (2 :: Int)
      s1 = y1 * z2 ^ (3 :: Int)
      s2 = y2 * z1 ^ (3 :: Int)
   in if u1 == u2
        then
          if s1 /= s2
            then PJIdentity
            else ecDouble p1
        else
          let h = u2 - u1
              r = s2 - s1
              x3 = r ^ (2 :: Int) - h ^ (3 :: Int) - 2 * u1 * h ^ (2 :: Int)
              y3 = r * (u1 * h ^ (2 :: Int) - x3) - s1 * h ^ (3 :: Int)
              z3 = h * z1 * z2
           in PJ x3 y3 z3

-- https://hyperelliptic.org/EFD/g1p/data/shortw/jacobian-0/addition
-- /madd-2007-bl
ecAddMixed :: PointJ -> Point -> PointJ
ecAddMixed PJIdentity (P x2 y2) = (PJ x2 y2 1)
ecAddMixed p Identity = p
ecAddMixed (PJ x1 y1 z1) (P x2 y2) =
  let z1z1 = z1 ^ (2 :: Int)
      u2 = x2 * z1z1
      s2 = y2 * z1 * z1z1
      h = u2 - x1
      hh = h ^ (2 :: Int)
      i = 4 * hh
      j = h * i
      r = 2 * (s2 - y1)
      v = x1 * i
      x3 = r ^ (2 :: Int) - j - 2 * v
      y3 = r * (v - x3) - 2 * y1 * j
      z3 = (z1 + h) ^ (2 :: Int) - z1z1 - hh
   in PJ x3 y3 z3

ecNegate :: PointJ -> PointJ
ecNegate PJIdentity = PJIdentity
ecNegate (PJ x y z) = (PJ x (Prelude.negate y) z)

ecDouble :: PointJ -> PointJ
ecDouble PJIdentity = PJIdentity
ecDouble (PJ x y z) =
  let s = 4 * x * y ^ (2 :: Int)
      m = 3 * x ^ (2 :: Int)
      x' = m ^ (2 :: Int) - 2 * s
      y' = m * (s - x') - 8 * y ^ (4 :: Int)
      z' = 2 * y * z
   in PJ x' y' z'

ecDoubleN :: Natural -> PointJ -> PointJ
ecDoubleN 0 p = p
ecDoubleN n p = ecDoubleN (pred n) (ecDouble p)

fromJacobian :: PointJ -> Point
fromJacobian PJIdentity = Identity
fromJacobian (PJ x y z) =
  let x' = x / z ^ (2 :: Int)
      y' = y / z ^ (3 :: Int)
   in P x' y'
