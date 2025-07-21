-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.StackUntyped
  ( SU (..),
    F,
    FNU,
    toTyped,
    fromTyped,
  )
where

import Alba.Dsl.V1.Common.FunctionState (FunctionState)
import Alba.Dsl.V1.Common.Stack (FNA, S (..))
import Alba.Vm.Common.OpcodeL2 (CodeL2)
import GHC.Stack (HasCallStack)

data SU = SU
  { c :: CodeL2,
    fs :: FunctionState
  }
  deriving (Show)

-- Applies HasCallStack so the type can be used for a VM function.
type F a = (HasCallStack) => a

type FNU = F (SU -> SU)

toTyped :: FNU -> FNA s alt s' alt'
toTyped prog (S c fs) = let (SU c' fs') = prog (SU c fs) in S c' fs'

fromTyped :: FNA s alt s' alt' -> FNU
fromTyped prog (SU c fs) = let (S c' fs') = prog (S c fs) in SU c' fs'
