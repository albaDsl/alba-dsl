-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Applicative
  ( liftA2Maybe,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Fn,
    StackEntry,
    begin,
    op2Drop,
    op2Dup,
    opBoolAnd,
    opIf,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Error (errCanNotHappen)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, rot, swap)
import Alba.Dsl.V1.Bch2026.Contract.TMaybe
  ( TMaybe,
    fromMaybe',
    isJust,
    just,
    nothing,
  )
import Alba.Dsl.V1.Bch2026.Lang (fn, invoke2, lambda0)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Prelude ()

liftA2Maybe ::
  (StackEntry a, StackEntry b) =>
  Fn (s > TLambda '[a, b] '[c] > TMaybe a > TMaybe b) (s > TMaybe c)
liftA2Maybe =
  fn
    ( begin
        . (op2Dup . isJust . swap . isJust . opBoolAnd)
        . opIf
          (fromJust . swap . fromJust . swap . rot . invoke2 . just)
          (op2Drop . drop . nothing)
    )

-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => Fn (s > TMaybe a) (s > a)
fromJust = lambda0 (errCanNotHappen) . swap . fromMaybe'
