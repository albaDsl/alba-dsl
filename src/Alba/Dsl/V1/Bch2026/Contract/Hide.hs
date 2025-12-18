-- Copyright (c) 2025 albaDsl

-- There is likely a better way to do solve this problem than this hide
-- mechanism.

module Alba.Dsl.V1.Bch2026.Contract.Hide where

import Alba.Dsl.V1.Bch2025
  ( FN,
    StackEntry,
    cast,
    castStack,
    type (>),
  )

data Hide a

instance StackEntry (Hide a)

hide :: FN (s > a) (s > Hide a)
hide = cast

nipHide :: FN (s > Hide a > b) (s > a > b)
nipHide = castStack

dropHide :: FN (s > Hide a) (s > a)
dropHide = castStack

hide2 :: FN (s > a > b) (s > Hide a > Hide b)
hide2 = castStack

nipHide2 :: FN (s > Hide a > Hide b > c > d) (s > a > b > c > d)
nipHide2 = castStack
