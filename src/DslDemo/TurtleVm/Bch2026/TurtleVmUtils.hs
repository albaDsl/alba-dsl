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

import Alba.Dsl.V1.Bch2026
  ( Bytes,
    FN,
    FNA,
    S (S),
    TBool,
    TBytes,
    TInt,
    bytes,
    cast,
    function,
    int,
    nat,
    opBin2Num,
    opCat,
    opDrop,
    opFalse,
    opGreaterThan,
    opNumEqual,
    opSize,
    opSplit,
    opVerify,
    opWhen,
    opWithin,
    progBytes,
    (#),
    type (>),
  )

vmError :: Bytes -> FNA s alt s' alt'
vmError msg = bytes msg # opFalse # opVerify # castStack

castStack :: FNA s alt s' alt'
castStack (S c fs) = let state = S c fs in state

-- Convert a positive value represented as a bytestring to a positive CashVm
-- integer.
toSigned :: FN (s > TBytes) (s > TInt)
toSigned = bytes [0] # opCat # opBin2Num

-- Convert a positive value [0, 255] represented as an CashVm integer to a
-- single byte bytestring.
fromSigned :: FN (s > TInt) (s > TBytes)
fromSigned =
  i2b # opSize # nat 1 # opGreaterThan # opWhen (nat 1 # opSplit # opDrop)
  where
    i2b :: FN (s > TInt) (s > TBytes)
    i2b = cast

unsupportedOp :: FN s (s > TBytes)
unsupportedOp = function (vmError "E1")

unsupportedOpBytes :: FN s (s > TBytes)
unsupportedOpBytes = progBytes unsupportedOp

inRange :: Integer -> Integer -> FN (s > TInt) (s > TBool)
inRange x y = int x # int y # opWithin

isSingleByteOp :: FN (s > TBytes) (s > TBytes > TBool)
isSingleByteOp = opSize # nat 1 # opNumEqual

isConditionalOp :: FN (s > TBytes) (s > TBool)
isConditionalOp = toSigned # int 0x63 # int 0x69 # opWithin

isOpDataOp :: FN (s > TBytes) (s > TBool)
isOpDataOp = toSigned # inRange 0x01 0x4c
