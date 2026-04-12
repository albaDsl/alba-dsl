-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmUtils
  ( vmError,
    toSigned,
    fromSigned,
    unsupportedOp,
    unsupportedOpBytes,
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
    cast,
    int,
    nat,
    opBin2Num,
    opCat,
    opGreaterThan,
    opNumEqual,
    opSize,
    opSplit,
    opWhen,
    opWithin,
    (∘),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Error (error)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop)
import Alba.Dsl.V1.Bch2026.Lang (fn, progCode)
import Alba.Dsl.V1.Bch2026.Stack (TCode)
import Prelude hiding (drop, error)

vmError :: Bytes -> FnA s alt s' alt'
vmError msg = bytes msg ∘ error

-- Convert a positive value represented as a bytestring to a positive CashVm
-- integer.
toSigned :: Fn (s > TBytes) (s > TInt)
toSigned = bytes [0] ∘ opCat ∘ opBin2Num

-- Convert a positive value [0, 255] represented as an CashVm integer to a
-- single byte bytestring.
fromSigned :: Fn (s > TInt) (s > TBytes)
fromSigned =
  i2b ∘ opSize ∘ nat 1 ∘ opGreaterThan ∘ opWhen (nat 1 ∘ opSplit ∘ drop)
  where
    i2b :: Fn (s > TInt) (s > TBytes)
    i2b = cast

unsupportedOp :: Fn s (s > TCode)
unsupportedOp = fn (vmError "E1")

unsupportedOpBytes :: Fn s (s > TCode)
unsupportedOpBytes = progCode unsupportedOp

inRange :: Integer -> Integer -> Fn (s > TInt) (s > TBool)
inRange x y = int x ∘ int y ∘ opWithin

isSingleByteOp :: Fn (s > TBytes) (s > TBytes > TBool)
isSingleByteOp = opSize ∘ nat 1 ∘ opNumEqual

isConditionalOp :: Fn (s > TBytes) (s > TBool)
isConditionalOp = toSigned ∘ int 0x63 ∘ int 0x69 ∘ opWithin

isOpDataOp :: Fn (s > TBytes) (s > TBool)
isOpDataOp = toSigned ∘ inRange 0x01 0x4c
