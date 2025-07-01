-- Copyright (c) 2025 albaDsl

-- Implementation modeled after: "Fast Prime Field Elliptic Curve Cryptography
-- with 256 Bit Primes", Shay Gueron, Vlad Krasnov.

module DslDemo.EllipticCurve.Native.Jacobian
  ( g,
    mul,
    toAffine,
    Point (..),
    PointJ (..),
    FieldElement (..),
  )
where

import DslDemo.EllipticCurve.Native.FieldElement (FieldElement (..))
import Numeric.Natural (Natural)

data Point = P !FieldElement !FieldElement | Identity
  deriving (Eq, Show)

data PointJ = PJ !FieldElement !FieldElement !FieldElement | PJIdentity
  deriving (Eq, Show)

g :: PointJ
g =
  PJ
    0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
    0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    1

add :: PointJ -> PointJ -> PointJ
add PJIdentity p = p
add p PJIdentity = p
add p1@(PJ x1 y1 z1) (PJ x2 y2 z2) =
  let u1 = x1 * z2 ^ (2 :: Int)
      u2 = x2 * z1 ^ (2 :: Int)
      s1 = y1 * z2 ^ (3 :: Int)
      s2 = y2 * z1 ^ (3 :: Int)
   in if u1 == u2
        then
          if s1 /= s2
            then PJIdentity
            else double p1
        else
          let h = u2 - u1
              r = s2 - s1
              x3 = r ^ (2 :: Int) - h ^ (3 :: Int) - 2 * u1 * h ^ (2 :: Int)
              y3 = r * (u1 * h ^ (2 :: Int) - x3) - s1 * h ^ (3 :: Int)
              z3 = h * z1 * z2
           in PJ x3 y3 z3

double :: PointJ -> PointJ
double PJIdentity = PJIdentity
double (PJ x y z) =
  let s = 4 * x * y ^ (2 :: Int)
      m = 3 * x ^ (2 :: Int)
      x' = m ^ (2 :: Int) - 2 * s
      y' = m * (s - x') - 8 * y ^ (4 :: Int)
      z' = 2 * y * z
   in PJ x' y' z'

mul :: Natural -> PointJ -> PointJ
mul _ PJIdentity = PJIdentity
mul n p = mul' n p PJIdentity

mul' :: Natural -> PointJ -> PointJ -> PointJ
mul' 0 _ r = r
mul' n p r =
  let r' = if odd n then add r p else r
      p' = double p
   in mul' (n `div` 2) p' r'

toAffine :: PointJ -> Point
toAffine PJIdentity = Identity
toAffine (PJ x y z) =
  let x' = x / z ^ (2 :: Int)
      y' = y / z ^ (3 :: Int)
   in P x' y'
