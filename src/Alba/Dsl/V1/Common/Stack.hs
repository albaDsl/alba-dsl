-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Stack
  ( S (..),
    Base,
    F,
    FnA,
    Fn,
    FnC,
    CFn,
    CFnA,
    (:|),
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
  )
where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
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

data S (s :: [Type]) (alt :: [Type]) = S
  { c :: CodeL3,
    fs :: FunctionState
  }

-- A stack with nothing on it.
type Base = '[]

-- Applies HasCallStack so the type can be used for a VM function.
type F a = (HasCallStack) => a

-- Function with main and alt stack types.
type FnA (s :: [Type]) (alt :: [Type]) (s' :: [Type]) (alt' :: [Type]) =
  F (S s alt -> S s' alt')

-- Function with alt stack constant.
type Fn (s :: [Type]) (s' :: [Type]) =
  forall alt. F (S s alt -> S s' alt)

-- Function with both stacks constant.
type FnC = forall s alt. F (S s alt -> S s alt)

-- Contract function (entry point). Allows for a non-clean alt stack.
type CFnA s a = F (S s Base -> S (Base > TBool) a)

-- Contract function (entry point).
type CFn s = F (S s Base -> S (Base > TBool) Base)

data (a :: [Type]) :| b :: Type

infixr 9 :|

type family CountStackBranches (stacks :: [Type]) :: Nat where
  CountStackBranches (s > a :| (b :: [Type])) = 1 + CountStackBranches b
  CountStackBranches (s > a :| (b :: Type)) = 1 + CountStackBranches '[b]
  CountStackBranches (s > a) = 1

type family Ref (xs :: [Type]) (idx :: Nat) :: Maybe Type where
  Ref '[] _ = TypeError ('Text "Access past known stack.")
  Ref (xs > (_ :| _)) _ =
    TypeError
      ('Text "Can't lookup named stack entries located below stack branches.")
  Ref (xs > x) 0 = 'Just x
  Ref (xs > x) idx = Ref xs (idx - 1)

type family Remove (xs :: [Type]) (idx :: Nat) :: [Type] where
  Remove '[] _ = '[]
  Remove (xs > _) 0 = xs
  Remove (xs > x) idx = Remove xs (idx - 1) > x
