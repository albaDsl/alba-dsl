-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Stack (TLambda, TLambdaUntyped) where

import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Data.Kind (Type)

data TLambda (args :: [Type]) (return :: [Type])

data TLambdaUntyped

instance StackEntry (TLambda (args :: [Type]) (return :: [Type]))

instance StackEntry TLambdaUntyped
