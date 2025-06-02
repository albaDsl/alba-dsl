-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( module Alba.Dsl.V1.Bch2025.Contract.Prelude,
    iterate,
  )
where

import Alba.Dsl.V1.Bch2025
  ( TNat,
    nat,
    op1SubUnsafe,
    opDrop,
    opFromAltStack,
    opNop,
    opToAltStack,
  )
import Alba.Dsl.V1.Bch2025.Contract.Prelude
import Alba.Dsl.V1.Bch2025.Ops (opDup, opNumEqual)
import Alba.Dsl.V1.Bch2026.Ops (opUntil)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.Stack (FNA, TBool)
import Numeric.Natural (Natural)
import Prelude hiding (iterate)

iterate ::
  forall s alt.
  Natural ->
  FNA s (alt > TNat) s (alt > TNat) ->
  FNA s alt s alt
iterate n fn =
  if n > 0
    then nat (fromIntegral n) # opUntil body # opDrop
    else opNop
  where
    body :: FNA (s > TNat) alt (s > TNat > TBool) alt
    body =
      begin
        # opToAltStack
        # fn
        # (opFromAltStack # op1SubUnsafe)
        # (opDup # nat 0 # opNumEqual)
