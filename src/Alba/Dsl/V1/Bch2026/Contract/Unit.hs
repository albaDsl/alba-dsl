-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Unit (TUnit, unit) where

import Alba.Dsl.V1.Bch2025 (Fn, StackEntry, cast, int, (#), type (>))

data TUnit

instance StackEntry TUnit

unit :: Fn s (s > TUnit)
unit = int 0 # cast
