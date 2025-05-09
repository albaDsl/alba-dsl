-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.TypeFamilies (Append, Reverse, Replicate) where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Data.Kind (Type)
import GHC.TypeLits (Nat, type (-))

type family Append base more where
  Append base '[] = base
  Append base (more > a) = Append base more > a

type family Reverse xs where
  Reverse xs = ReverseAcc xs '[]

type family ReverseAcc xs acc where
  ReverseAcc '[] acc = acc
  ReverseAcc (xs > x) acc = ReverseAcc xs (acc > x)

type family Replicate (n :: Nat) (t :: Type) where
  Replicate 0 _ = '[]
  Replicate n t = Replicate (n - 1) t > t
