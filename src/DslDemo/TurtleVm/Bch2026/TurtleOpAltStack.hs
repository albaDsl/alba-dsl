-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleOpAltStack
  ( toAltStack,
    fromAltStack,
  )
where

import Alba.Dsl.V1.Bch2025.OpsUntyped (opFromAltStack, opSwap, opToAltStack)
import Alba.Dsl.V1.Bch2026 (FNA, (#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, fromTyped)
import DslDemo.TurtleVm.Bch2026.TurtleVmState (getState, putState)

toAltStack :: FNU
toAltStack = ft getState # opSwap # opToAltStack # ft putState

fromAltStack :: FNU
fromAltStack = ft getState # opFromAltStack # opSwap # ft putState

ft :: FNA s alt s' alt' -> FNU
ft = fromTyped
