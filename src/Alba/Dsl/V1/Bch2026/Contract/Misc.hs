-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Misc
  ( iterate,
  )
where

import Alba.Dsl.V1.Bch2025
  ( TNat,
    nat,
    opFromAltStack,
    opNop,
    opToAltStack,
  )
import Alba.Dsl.V1.Bch2025.Contract.Prelude (nat1SubUnsafe)
import Alba.Dsl.V1.Bch2026 (FnA, Stack (..), TBool)
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqCoreInstances ()
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup)
import Alba.Dsl.V1.Bch2026.Ops (opUntil)
import Alba.Dsl.V1.Common.Lang (begin, (.))
import Numeric.Natural (Natural)
import Prelude hiding (drop, iterate, (.))

iterate ::
  forall s alt.
  Natural ->
  FnA s (alt :> TNat) s (alt :> TNat) ->
  FnA s alt s alt
iterate n f =
  if n > 0
    then nat (fromIntegral n) . opUntil body . drop
    else opNop
  where
    body :: FnA (s :> TNat) alt (s :> TNat :> TBool) alt
    body =
      begin
        . opToAltStack
        . f
        . (opFromAltStack . nat1SubUnsafe)
        . (dup . nat 0 . equal)
