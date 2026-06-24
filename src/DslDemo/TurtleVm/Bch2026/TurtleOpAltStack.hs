-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleOpAltStack
  ( toAltStack,
    fromAltStack,
  )
where

import Alba.Dsl.V1.Bch2026 (FnA)
import Alba.Dsl.V1.Bch2026.OpsUntyped (opFromAltStack, opSwap, opToAltStack)
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped, (.))
import DslDemo.TurtleVm.Bch2026.TurtleVmState (getState, putState)
import Prelude ()

toAltStack :: FnU
toAltStack = ft getState . opSwap . opToAltStack . ft putState

fromAltStack :: FnU
fromAltStack = ft getState . opFromAltStack . opSwap . ft putState

ft :: FnA s alt s' alt' -> FnU
ft = fromTyped
