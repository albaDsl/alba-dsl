-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Stack
  ( Stack (..),
    S (..),
    F,
    FnA,
    Fn,
    FnC,
    CFn,
    CFnA,
    (:|),
    Append,
    Replicate,
    ListToStack,
    CountStackBranches,
    Ref,
    Remove,
    TUnknown,
    TInt,
    TNat,
    TBool,
    TBytes,
    TSig,
    TPubKey,
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
data TUnknown
data TInt
data TNat
data TBool
data TBytes
data TSig
data TPubKey
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

data (a :: Stack) :| b :: Type

infixr 9 :|

type family Append s s' where
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
