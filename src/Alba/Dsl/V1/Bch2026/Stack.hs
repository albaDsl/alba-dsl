-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Stack (TCode, TLambda, TLambdaUntyped) where

import Alba.Dsl.V1.Bch2025.Stack (StackBytes, StackEntry)
import Data.Kind (Type)

data TCode

data TLambda (args :: [Type]) (return :: [Type])

data TLambdaUntyped

instance StackEntry TCode

instance StackEntry (TLambda (args :: [Type]) (return :: [Type]))

instance StackEntry TLambdaUntyped

instance StackBytes TCode
