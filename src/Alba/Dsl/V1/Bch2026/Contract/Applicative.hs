-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Applicative
  ( liftA2Maybe,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    begin,
    op2Drop,
    op2Dup,
    opBoolAnd,
    opIf,
    (.),
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
import Alba.Dsl.V1.Bch2026.Lang (fn)
import Alba.Dsl.V1.Bch2026.QuotationsB (invoke2, quot0)
import Alba.Dsl.V1.Common.Stack (TQuotB)
import Prelude ()

liftA2Maybe ::
  (StackEntry a, StackEntry b) =>
  Fn (s :> TQuotB '[a, b] '[c] :> TMaybe a :> TMaybe b) (s :> TMaybe c)
liftA2Maybe =
  fn
    ( begin
        . (op2Dup . isJust . swap . isJust . opBoolAnd)
        . opIf
          (fromJust . swap . fromJust . swap . rot . invoke2 . just)
          (op2Drop . drop . nothing)
    )

-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => Fn (s :> TMaybe a) (s :> a)
fromJust = quot0 (errCanNotHappen) . swap . fromMaybe'
