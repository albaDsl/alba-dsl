module Alba.Dsl.V1.Bch2026.Contract.Quotations where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    TQuotA,
    TQuotB,
    cast,
  )
import Prelude ()

-- A simple cast is enough since the runtimeLib 'invoke' function has a special
-- case for plain function references.
quotBtoA :: Fn (s' :> TQuotB a b) (s' :> TQuotA a b)
quotBtoA = cast
