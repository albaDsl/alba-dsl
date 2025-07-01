-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Constants (g, a, b, p, n) where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.Point (TPoint, pushPoint)
import Numeric.Natural (Natural)

g :: FN s (s > TPoint)
g = pushPoint x y
  where
    x = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
    y = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8

a :: Integer
a = 0

b :: Integer
b = 7

p :: Natural
p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

n :: Integer
n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
