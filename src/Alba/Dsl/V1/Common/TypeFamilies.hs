-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.TypeFamilies (Reverse) where

type family Reverse (xs :: [k]) where
  Reverse xs = ReverseAcc xs '[]

type family ReverseAcc xs acc where
  ReverseAcc '[] acc = acc
  ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)
