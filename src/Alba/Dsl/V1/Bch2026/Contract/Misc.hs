-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Misc
  ( natSub,
    natSubUnsafe,
    ifZero,
    isZero,
    null,
    nat1Sub,
    nat1SubUnsafe,
    iterate,
    functionIdOffset,
    addToUnsigned,
    pad,
    unpad,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    FnA,
    Stack (..),
    StackNum,
    TBool,
    TBytes,
    TFunctionId,
    TNat,
    bytes,
    cast,
    emptyProg,
    fn,
    i2n,
    i2nUnsafe,
    int,
    n2i,
    nat,
    ns2,
    op0,
    op1Sub,
    opBin2Num,
    opCat,
    opEqual,
    opGreaterThan,
    opIf,
    opNum2Bin,
    opNumEqual,
    opReverseBytes,
    opSize,
    opSplit,
    opSub,
    opSwap,
    opWhen,
    roll,
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqCoreInstances ()
import Alba.Dsl.V1.Bch2026.Contract.Error (errPartialFunction)
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..), add)
import Alba.Dsl.V1.Bch2026.Contract.Ord (Ord (lessThanOrEqual))
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, fromAlt, swap, toAlt)
import Alba.Dsl.V1.Bch2026.Ops (opUntil)
import Alba.Dsl.V1.Common.Lang (begin, (.))
import Numeric.Natural (Natural)
import Prelude hiding (drop, iterate, null, (.))

natSub :: Fn (s :> TNat :> TNat) (s :> TNat)
natSub = n2i . opSwap . n2i . opSwap . opSub . i2n

natSubUnsafe :: Fn (s :> TNat :> TNat) (s :> TNat)
natSubUnsafe = n2i . opSwap . n2i . opSwap . opSub . i2nUnsafe

nat1Sub :: Fn (s :> TNat) (s :> TNat)
nat1Sub = n2i . op1Sub . i2n

nat1SubUnsafe :: Fn (s :> TNat) (s :> TNat)
nat1SubUnsafe = n2i . op1Sub . i2nUnsafe

ifZero ::
  (StackNum x1) =>
  FnA s alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s :> x1) alt s' alt'
ifZero ifOps elseOps = isZero . opIf ifOps elseOps

isZero :: (StackNum x1) => Fn (s :> x1) (s :> TBool)
isZero = op0 . opNumEqual

null :: Fn (s :> TBytes) (s :> TBool)
null = bytes [] . opEqual

iterate ::
  forall s alt.
  Natural ->
  FnA s (alt :> TNat) s (alt :> TNat) ->
  FnA s alt s alt
iterate n f =
  if n > 0 then nat (fromIntegral n) . opUntil body . drop else emptyProg
  where
    body :: FnA (s :> TNat) alt (s :> TNat :> TBool) alt
    body = toAlt . f . fromAlt . nat1SubUnsafe . dup . nat 0 . equal

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

pad :: Fn (s :> a :> TNat) (s :> TBytes)
pad =
  fn
    ( begin
        . (ns2 #val #toSize)
        . (roll #val . valToBytes . opSize . toTag . swap . opCat . opSize)
        . (roll #toSize . swap . sub . extend)
    )
  where
    valToBytes :: Fn (s :> a) (s :> TBytes)
    valToBytes = cast

    -- Tag is stored as an unsigned integer.
    toTag :: Fn (s :> TNat) (s :> TBytes)
    toTag =
      begin
        . (dup . nat maxValLength . lessThanOrEqual)
        . opIf (n2b . nat 1 . opSplit . drop) errPartialFunction

    n2b :: Fn (s :> TNat) (s :> TBytes)
    n2b = cast

    extend :: Fn (s :> TBytes :> TNat) (s :> TBytes)
    extend = int 0 . swap . opNum2Bin . opCat

    maxValLength = 256

tagSize :: Fn s (s :> TNat)
tagSize = nat 1

unpad :: Fn (s :> TBytes) (s :> a)
unpad = fn (tagSize . opSplit . swap . toSigned . opSplit . drop . bytesToVal)
  where
    toSigned :: Fn (s :> TBytes) (s :> TNat)
    toSigned = bytes [0] . opCat . opBin2Num . i2nUnsafe

    bytesToVal :: Fn (s :> TBytes) (s :> a)
    bytesToVal = cast
