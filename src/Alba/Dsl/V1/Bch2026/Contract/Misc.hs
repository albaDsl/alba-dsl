-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Misc
  ( iterate,
    functionIdOffset,
    addToUnsigned,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Prelude (nat1SubUnsafe)
import Alba.Dsl.V1.Bch2026
  ( Fn,
    FnA,
    Stack (..),
    TBool,
    TBytes,
    TFunctionId,
    TNat,
    bytes,
    cast,
    fn,
    i2nUnsafe,
    nat,
    opBin2Num,
    opCat,
    opFromAltStack,
    opGreaterThan,
    opIf,
    opNop,
    opReverseBytes,
    opSize,
    opSplit,
    opToAltStack,
    opWhen,
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqCoreInstances ()
import Alba.Dsl.V1.Bch2026.Contract.Integral (add)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, swap)
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

-- Only for use with Local function IDs which are unsigned integers.
functionIdOffset :: Fn (s :> TFunctionId :> TNat) (s :> TFunctionId)
functionIdOffset = swap . f2b . addToUnsigned . b2f
  where
    f2b :: Fn (s :> TFunctionId) (s :> TBytes)
    f2b = cast

    b2f :: Fn (s :> TBytes) (s :> TFunctionId)
    b2f = cast

-- Add a TNat in standard signed representation to an unsigned Nat represented
-- as a bytestring.
addToUnsigned :: Fn (s :> TNat :> TBytes) (s :> TBytes)
addToUnsigned = fn (toSigned . add . fromSigned)
  where
    -- To non-negative number in standard signed VM representation.
    toSigned :: Fn (s :> TBytes) (s :> TNat)
    toSigned = bytes [0] . opCat . opBin2Num . i2nUnsafe

    -- from Non-negative number in standard signed VM representation.
    fromSigned :: Fn (s :> TNat) (s :> TBytes)
    fromSigned =
      begin
        . (n2b . opSize . nat 1 . opGreaterThan)
        . opWhen
          ( begin
              . (opReverseBytes . nat 1 . opSplit . opReverseBytes . swap)
              . (dup . bytes [0] . equal . opIf drop opCat)
          )

    n2b :: Fn (s :> TNat) (s :> TBytes)
    n2b = cast
