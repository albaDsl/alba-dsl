-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVmUtils
  ( vmError,
    toSigned,
    unsupportedOp,
    inRange,
    isSingleByteOp,
    isConditionalOp,
    isOpDataOp,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    Fn,
    FnA,
    TBool,
    TBytes,
    TInt,
    bytes,
    castStack,
    int,
    nat,
    opBin2Num,
    opCat,
    opFalse,
    opNumEqual,
    opSize,
    opVerify,
    opWithin,
    (∘),
    type (>),
  )

vmError :: Bytes -> FnA s alt s' alt'
vmError msg = bytes msg ∘ opFalse ∘ opVerify ∘ castStack

-- Convert a positive value [0, 255] represented as a single byte to a positive
-- CashVm integer.
toSigned :: Fn (s > TBytes) (s > TInt)
toSigned = bytes [0] ∘ opCat ∘ opBin2Num

unsupportedOp :: Fn s (s > TBytes)
unsupportedOp = vmError "E1"

inRange :: Integer -> Integer -> Fn (s > TInt) (s > TBool)
inRange x y = int x ∘ int y ∘ opWithin

isSingleByteOp :: Fn (s > TBytes) (s > TBytes > TBool)
isSingleByteOp = opSize ∘ nat 1 ∘ opNumEqual

isConditionalOp :: Fn (s > TBytes) (s > TBool)
isConditionalOp = toSigned ∘ int 0x63 ∘ int 0x69 ∘ opWithin

isOpDataOp :: Fn (s > TBytes) (s > TBool)
isOpDataOp = toSigned ∘ inRange 0x01 0x4c
