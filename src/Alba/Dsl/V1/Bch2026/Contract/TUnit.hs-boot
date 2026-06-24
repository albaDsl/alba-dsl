-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TUnit (TUnit, unit) where

import Alba.Dsl.V1.Bch2026 (Fn, Stack (..), StackEntry)

data TUnit

instance StackEntry TUnit

unit :: Fn s (s :> TUnit)
