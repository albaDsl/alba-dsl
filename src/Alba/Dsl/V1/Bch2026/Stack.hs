-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Stack
  ( TCode,
    TFunctionId,
    TLambda,
    TLambdaUntyped,
    TRuntimeState,
    Env,
  )
where

import Alba.Dsl.V1.Bch2025.Stack (StackBytes, StackEntry)
import Alba.Dsl.V1.Common.Stack (FnA, Stack (..))
import Data.Kind (Type)

data TCode

data TFunctionId

data TLambda (args :: [Type]) (return :: [Type])

data TLambdaUntyped

data TRuntimeState

instance StackEntry TCode

instance StackEntry TFunctionId

instance StackEntry (TLambda (args :: [Type]) (return :: [Type]))

instance StackEntry TLambdaUntyped

instance StackEntry TRuntimeState

instance StackBytes TCode

type Env (s :: Stack) (s' :: Stack) =
  FnA s (Base :> TRuntimeState) s' (Base :> TRuntimeState)
