-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Native.FieldElement (FieldElement (..)) where

import DslDemo.EllipticCurve.Native.Constants (pMod)

newtype FieldElement = FieldElement Integer
  deriving (Eq, Show)

instance Num FieldElement where
  FieldElement x + FieldElement y = fromInteger $ x + y
  FieldElement x * FieldElement y = fromInteger $ x * y
  abs x = x
  signum _ = 1
  negate (FieldElement x) = fromInteger $ negate x
  fromInteger x = FieldElement (x `mod` pMod)

instance Fractional FieldElement where
  recip x = x ^ (pMod - 2)
  fromRational _ = error "FieldElement: fromRational"
