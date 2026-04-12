-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Error where

import Alba.Dsl.V1.Bch2026
import Prelude ()

error :: FnA (s > TBytes) alt s' alt'
error = opFalse ∘ opVerify ∘ castStack

error' :: FnA s alt s' alt'
error' = opFalse ∘ opVerify ∘ castStack

errCanNotHappen :: FnA (s > TBytes) alt s' alt'
errCanNotHappen = bytes "E0" ∘ error

errPartialFunction :: FnA (s > TBytes) alt s' alt'
errPartialFunction = bytes "E1" ∘ error
