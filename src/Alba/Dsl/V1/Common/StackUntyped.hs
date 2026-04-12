-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.StackUntyped
  ( (.),
    (∘),
    SU (..),
    F,
    FnU,
    toTyped,
    fromTyped,
  )
where

import Alba.Dsl.V1.Common.FunctionState (FunctionState)
import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3)
import Alba.Dsl.V1.Common.Stack (FnA, S (..))
import GHC.Stack (HasCallStack)
import Prelude ()
import Prelude qualified as P

(.) :: (SU -> SU) -> (SU -> SU) -> (SU -> SU)
(.) op1 op2 = op2 P.. op1

(∘) :: (SU -> SU) -> (SU -> SU) -> (SU -> SU)
(∘) = (.)

data SU = SU
  { c :: CodeL3,
    fs :: FunctionState
  }

-- Applies HasCallStack so the type can be used for a VM function.
type F a = (HasCallStack) => a

type FnU = F (SU -> SU)

toTyped :: FnU -> FnA s alt s' alt'
toTyped prog (S c fs) = let (SU c' fs') = prog (SU c fs) in S c' fs'

fromTyped :: FnA s alt s' alt' -> FnU
fromTyped prog (SU c fs) = let (S c' fs') = prog (S c fs) in SU c' fs'
