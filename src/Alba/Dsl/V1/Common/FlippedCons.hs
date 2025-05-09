-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FlippedCons (type (>)) where

type a > b = (b ': a)

infixl 8 >
