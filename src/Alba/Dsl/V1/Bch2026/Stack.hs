-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Stack
  ( TCode,
    TLambda,
    TLambdaUntyped,
    TRuntimeState,
    ENV,
  )
where

import Alba.Dsl.V1.Bch2025.Stack (StackBytes, StackEntry)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (Base, FNA)
import Data.Kind (Type)

data TCode

data TLambda (args :: [Type]) (return :: [Type])

data TLambdaUntyped

data TRuntimeState

instance StackEntry TCode

instance StackEntry (TLambda (args :: [Type]) (return :: [Type]))

instance StackEntry TLambdaUntyped

instance StackEntry TRuntimeState

instance StackBytes TCode

type ENV (s :: [Type]) (s' :: [Type]) =
  FNA s (Base > TRuntimeState) s' (Base > TRuntimeState)
