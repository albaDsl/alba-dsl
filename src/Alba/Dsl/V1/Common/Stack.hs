-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Stack
  ( S (..),
    Base,
    FNA,
    FN,
    FNC,
    CFN,
    CFNA,
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
import Alba.Vm.Common.OpcodeL2 (CodeL2)
import Data.Kind (Type)
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
  { c :: CodeL2,
    slot :: Int
  }
  deriving (Show)

-- A stack with nothing on it.
type Base = '[]

-- Function with main and alt stack types.
type FNA (s :: [Type]) (alt :: [Type]) (s' :: [Type]) (alt' :: [Type]) =
  S s alt -> S s' alt'

-- Function with alt stack constant.
type FN (s :: [Type]) (s' :: [Type]) =
  forall alt. S s alt -> S s' alt

-- Function with both stacks constant.
type FNC = forall s alt. S s alt -> S s alt

-- Contract function with alt stack constant.
type CFN s = S s Base -> S (Base > TBool) Base

-- Contract function with main and alt stack types.
type CFNA s a = S s Base -> S (Base > TBool) a

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
