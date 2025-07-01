-- Copyright (c) 2025 albaDsl

-- Credit to https://oscarnajera.com/2021/07/
-- elliptic-curves-on-finite-fields-bitcoin-haskell/
-- for inspiration.

module DslDemo.EllipticCurve.Native.Affine
  ( g,
    mul,
    Point (..),
    FieldElement (..),
  )
where

import DslDemo.EllipticCurve.Native.FieldElement (FieldElement (..))
import Numeric.Natural (Natural)

data Point = P !FieldElement !FieldElement | Identity
  deriving (Eq, Show)

g :: Point
g =
  P
    0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
    0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8

add :: Point -> Point -> Point
add Identity p = p
add p Identity = p
add (P x y) (P x' y')
  | x == x' && y /= y' = Identity
  | x == x' && y == 0 = Identity
add p1 p2 | p1 == p2 = double p1
add (P x y) (P x' y') =
  let l = (y - y') / (x - x')
      x'' = (l * l) - (x + x')
      y'' = l * (x - x'') - y
   in P x'' y''

double :: Point -> Point
double Identity = Identity
double (P x y) =
  let l = (3 * (x * x)) / (2 * y)
      x' = (l * l) - 2 * x
      y' = l * (x - x') - y
   in P x' y'

mul :: Natural -> Point -> Point
mul _ Identity = Identity
mul n p = mul' n p Identity

mul' :: Natural -> Point -> Point -> Point
mul' 0 _ r = r
mul' n p r =
  let r' = if odd n then add r p else r
      p' = double p
   in mul' (n `div` 2) p' r'
