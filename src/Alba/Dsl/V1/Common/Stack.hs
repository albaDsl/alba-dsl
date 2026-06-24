-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Stack
  ( (:|),
    Append,
    CFn,
    CFnA,
    CountStackBranches,
    Env,
    F,
    Fn,
    FnA,
    FnC,
    ListToStack,
    Ref,
    Remove,
    Replicate,
    S (..),
    Stack (..),
    StackBool,
    StackBytes,
    StackEntry,
    StackEquatable,
    StackInt,
    StackNat,
    StackNum,
    TBool,
    TBytes,
    TCode,
    TFunctionId,
    THash160,
    THash256,
    TInt,
    TNat,
    TPubKey,
    TQuotA,
    TQuotB,
    TRipemd160,
    TRuntimeState,
    TSha1,
    TSha256,
    TSig,
    TUnknown,
    cast,
    castStack,
  )
where

import Alba.Dsl.V1.Common.FunctionState (FunctionState)
import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3)
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage (Text), Nat, TypeError, type (+), type (-))

{- ORMOLU_DISABLE -}
data TBool
data TBytes
data TCode
data TFunctionId
data THash160
data THash256
data TInt
data TNat
data TPubKey
data TQuotA (args :: [Type]) (return :: [Type])
data TQuotB (args :: [Type]) (return :: [Type])
data TQuotUntyped
data TRipemd160
data TRuntimeState
data TSha1
data TSha256
data TSig
data TUnknown

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
instance StackEntry (TQuotA (args :: [Type]) (return :: [Type]))
instance StackEntry (TQuotB (args :: [Type]) (return :: [Type]))
instance StackEntry TQuotUntyped
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

data Stack
  = Base -- Empty stack.
  | Stack :> Type -- Cons operator.

infixl 5 :>

data S (s :: Stack) (alt :: Stack) = S
  { c :: CodeL3,
    fs :: FunctionState
  }

-- Applies HasCallStack so the type can be used for a VM function.
type F a = (HasCallStack) => a

-- Function with main and alt stack types.
type FnA (s :: Stack) (alt :: Stack) (s' :: Stack) (alt' :: Stack) =
  F (S s alt -> S s' alt')

-- Function with alt stack constant.
type Fn (s :: Stack) (s' :: Stack) =
  forall alt. F (S s alt -> S s' alt)

-- Function with both stacks constant.
type FnC = forall s alt. F (S s alt -> S s alt)

-- Contract function (entry point). Allows for a non-clean alt stack.
type CFnA s a = F (S s Base -> S (Base :> TBool) a)

-- Contract function (entry point).
type CFn s = F (S s Base -> S (Base :> TBool) Base)

type Env (s :: Stack) (s' :: Stack) =
  FnA s (Base :> TRuntimeState) s' (Base :> TRuntimeState)

data (a :: Stack) :| b :: Type

infixr 9 :|

type family Append (s :: Stack) (s' :: Stack) where
  Append s Base = s
  Append s (s' :> a) = (Append s s') :> a

type family Replicate (n :: Nat) (t :: Type) where
  Replicate 0 _ = Base
  Replicate n t = Replicate (n - 1) t :> t

type family ListToStack (list :: [Type]) :: Stack where
  ListToStack '[] = 'Base
  ListToStack (a ': s) = ListToStack s :> a

type family CountStackBranches (s :: Stack) :: Nat where
  CountStackBranches (s :> a :| (b :: Stack)) = 1 + CountStackBranches b
  CountStackBranches (s :> a :| (b :: Type)) =
    1 + CountStackBranches (Base :> b)
  CountStackBranches (s :> a) = 1

type family Ref (s :: Stack) (idx :: Nat) :: Maybe Type where
  Ref Base _ = TypeError ('Text "Access past known stack.")
  Ref (s :> (_ :| _)) _ =
    TypeError
      ('Text "Can't lookup named stack entries located below stack branches.")
  Ref (s :> x) 0 = 'Just x
  Ref (s :> x) idx = Ref s (idx - 1)

type family Remove (s :: Stack) (idx :: Nat) :: Stack where
  Remove Base _ = Base
  Remove (s :> _) 0 = s
  Remove (s :> x) idx = Remove s (idx - 1) :> x

cast :: Fn (s :> t1) (s :> t2)
cast (S c fs) = let state = S c fs in state

castStack :: FnA s alt s' alt'
castStack (S c fs) = let state = S c fs in state
