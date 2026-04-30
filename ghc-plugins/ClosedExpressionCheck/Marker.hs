-- Copyright (c) 2026 albaDsl

module ClosedExpressionCheck.Marker (closedCheck) where

-- Identity function used as a marker.
--
-- The source pass of the plugin wraps targeted arguments in `closedCheck`
-- during renaming; the Core pass then finds these wrappers after type-class
-- elaboration, verifies that the wrapped expression is closed (also with
-- respect to typeclass dictionaries), and strips the wrapper so there is no
-- runtime cost.
--
-- NOINLINE is essential: without it the simplifier may erase the marker before
-- the Core pass runs. This module intentionally has no dependencies beyond
-- `base` so that users of the DSL do not transitively pull in the `ghc`
-- package.
closedCheck :: a -> a
closedCheck x = x
{-# NOINLINE closedCheck #-}
