-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Conditionals (case', cond) where

import Alba.Dsl.V1.Bch2026

case' ::
  forall s t alt s' alt'.
  (StackEntry t) =>
  [ ( S (s :> t :> t) alt -> S (s :> t :> TBool) alt,
      S (s :> t) alt -> S s' alt'
    )
  ] ->
  (S (s :> t) alt -> S s' alt') ->
  (S (s :> t) alt -> S s' alt')
case' [] def st = def st
case' ((test, result) : rest) def st =
  (opDup ∘ test ∘ opIf result (case' rest def)) st

cond ::
  forall s alt s' alt'.
  [(S s alt -> S (s :> TBool) alt, S s alt -> S s' alt')] ->
  (S s alt -> S s' alt') ->
  (S s alt -> S s' alt')
cond [] def st = def st
cond ((test, result) : rest) def st = (test ∘ opIf result (cond rest def)) st
