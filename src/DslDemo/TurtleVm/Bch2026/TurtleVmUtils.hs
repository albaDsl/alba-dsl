-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmUtils
  ( vmError,
    toSigned,
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
    function,
    int,
    nat,
    opBin2Num,
    opCat,
    opFalse,
    opNumEqual,
    opSize,
    opVerify,
    opWithin,
    progBytes,
    (#),
    type (>),
  )

vmError :: Bytes -> FNA s alt s' alt'
vmError msg = bytes msg # opFalse # opVerify # castStack

castStack :: FNA s alt s' alt'
castStack (S c fs) = let state = S c fs in state

toSigned :: FN (s > TBytes) (s > TInt)
toSigned = bytes [0] # opCat # opBin2Num

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
