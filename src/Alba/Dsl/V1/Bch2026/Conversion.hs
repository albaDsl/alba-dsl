-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Conversion where

import Alba.Dsl.V1.Bch2026.Lang (nat)
import Alba.Dsl.V1.Bch2026.Ops (opDup, opGreaterThanOrEqual, opVerify)
import Alba.Dsl.V1.Common.Lang ((∘))
import Alba.Dsl.V1.Common.Stack (Fn, Stack (..), TInt, TNat, cast)

n2i :: Fn (s :> TNat) (s :> TInt)
n2i = cast

i2n :: Fn (s :> TInt) (s :> TNat)
i2n = cast ∘ opDup ∘ nat 0 ∘ opGreaterThanOrEqual ∘ opVerify

-- Only use as an optimization when the TInt value is guaranteed to be
-- non-negative.
i2nUnsafe :: Fn (s :> TInt) (s :> TNat)
i2nUnsafe = cast
