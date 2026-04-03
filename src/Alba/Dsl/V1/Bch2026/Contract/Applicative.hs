-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Applicative
  ( liftA2Maybe,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    StackEntry,
    TLambda,
    begin,
    bytes,
    cast,
    fn,
    invoke2,
    lambda0,
    op2Drop,
    op2Dup,
    opBoolAnd,
    opFalse,
    opIf,
    opVerify,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Maybe
  ( TMaybe,
    fromMaybe',
    isJust,
    just,
    nothing,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, rot, swap)
import Prelude ()

liftA2Maybe ::
  (StackEntry a, StackEntry b) =>
  Fn (s > TLambda '[a, b] '[c] > TMaybe a > TMaybe b) (s > TMaybe c)
liftA2Maybe =
  fn
    ( begin
        # (op2Dup # isJust # swap # isJust # opBoolAnd)
        # opIf
          (fromJust # swap # fromJust # swap # rot # invoke2 # just)
          (op2Drop # drop # nothing)
    )

-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => Fn (s > TMaybe a) (s > a)
fromJust = err # swap # fromMaybe'
  where
    err = lambda0 (bytes "E0" # opFalse # opVerify # cast)
