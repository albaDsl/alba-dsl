-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Stack
  ( StackBool,
    StackBytes,
    StackEntry,
    StackEquatable,
    StackInt,
    StackNat,
    StackNum,
    THash160,
    THash256,
    TRipemd160,
    TSha1,
    TSha256,
    TCode,
    TFunctionId,
    TLambda,
    TLambdaUntyped,
    TRuntimeState,
    Env,
  )
where

import Alba.Dsl.V1.Common.Stack
  ( FnA,
    Stack (..),
    TBool,
    TBytes,
    TInt,
    TNat,
    TPubKey,
    TSig,
    TUnknown,
  )
import Data.Kind (Type)

{- ORMOLU_DISABLE -}
data TRipemd160
data TSha1
data TSha256
data THash160
data THash256
data TCode
data TFunctionId
data TLambda (args :: [Type]) (return :: [Type])
data TLambdaUntyped
data TRuntimeState

class StackEntry a
class StackEntry a => StackNum a
class StackEntry a => StackInt a
class StackEntry a => StackNat a
class StackEntry a => StackBool a
class StackEntry a => StackBytes a
class StackEntry a => StackEquatable a

instance StackEntry TUnknown
instance StackEntry TInt
instance StackEntry TNat
instance StackEntry TBool
instance StackEntry TBytes
instance StackEntry TRipemd160
instance StackEntry TSha1
instance StackEntry TSha256
instance StackEntry THash160
instance StackEntry THash256
instance StackEntry TPubKey
instance StackEntry TSig

instance StackNum TInt
instance StackNum TNat

instance StackInt TInt

instance StackNat TNat

instance StackBool TBool

instance StackBytes TBytes
instance StackBytes TRipemd160
instance StackBytes TSha1
instance StackBytes TSha256
instance StackBytes THash256
instance StackBytes THash160
instance StackBytes TSig
instance StackBytes TPubKey
instance StackEntry TCode
instance StackEntry TFunctionId
instance StackEntry (TLambda (args :: [Type]) (return :: [Type]))
instance StackEntry TLambdaUntyped
instance StackEntry TRuntimeState
instance StackBytes TCode

instance StackEquatable TBool
instance StackEquatable TBytes
instance StackEquatable TRipemd160
instance StackEquatable TSha1
instance StackEquatable TSha256
instance StackEquatable THash256
instance StackEquatable THash160
instance StackEquatable TSig
instance StackEquatable TPubKey
{- ORMOLU_ENABLE -}

type Env (s :: Stack) (s' :: Stack) =
  FnA s (Base :> TRuntimeState) s' (Base :> TRuntimeState)
