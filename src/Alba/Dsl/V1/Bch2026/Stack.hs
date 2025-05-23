-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Stack (TLambda, TLambdaUntyped) where

import Alba.Dsl.V1.Bch2025.Stack (StackEntry)

data TLambda s

data TLambdaUntyped

instance StackEntry (TLambda s)

instance StackEntry TLambdaUntyped
