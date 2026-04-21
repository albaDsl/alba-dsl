-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Error where

import Alba.Dsl.V1.Bch2026
  ( FnA,
    Stack (..),
    TBytes,
    bytes,
    castStack,
    opFalse,
    opVerify,
    (.),
  )
import Prelude ()

error :: FnA (s :> TBytes) alt s' alt'
error = opFalse . opVerify . castStack

error' :: FnA s alt s' alt'
error' = opFalse . opVerify . castStack

errCanNotHappen :: FnA s alt s' alt'
errCanNotHappen = bytes "E0" . error

errPartialFunction :: FnA s alt s' alt'
errPartialFunction = bytes "E1" . error
