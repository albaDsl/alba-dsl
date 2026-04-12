-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2025.Conversion where

import Alba.Dsl.V1.Bch2025.Lang (nat)
import Alba.Dsl.V1.Bch2025.Ops (opDup, opGreaterThanOrEqual, opVerify)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.Stack (Fn, TInt, TNat, cast)

n2i :: Fn (s > TNat) (s > TInt)
n2i = cast

i2n :: Fn (s > TInt) (s > TNat)
i2n = cast # opDup # nat 0 # opGreaterThanOrEqual # opVerify

-- Only use as an optimization when the TInt value is guaranteed to be
-- non-negative.
i2nUnsafe :: Fn (s > TInt) (s > TNat)
i2nUnsafe = cast
