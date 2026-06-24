-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Native.FieldElement (FieldElement (..)) where

import Control.DeepSeq (NFData)
import DslDemo.EllipticCurve.Constants (p)
import GHC.Generics (Generic)

data FieldElement = FieldElement Integer
  deriving (Eq, Show, NFData, Generic)

instance Num FieldElement where
  FieldElement x + FieldElement y = fromInteger $ x + y
  FieldElement x * FieldElement y = fromInteger $ x * y
  abs x = x
  signum _ = 1
  negate (FieldElement x) = fromInteger $ negate x
  fromInteger x = FieldElement (x `mod` p)

instance Fractional FieldElement where
  recip x = x ^ (p - 2)
  fromRational _ = error "FieldElement: fromRational"
