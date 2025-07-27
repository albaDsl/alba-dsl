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
    FN,
    FNA,
    S (S),
    TBool,
    TBytes,
    TInt,
    bytes,
    int,
    nat,
    opBin2Num,
    opCat,
    opFalse,
    opNumEqual,
    opSize,
    opVerify,
    opWithin,
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
unsupportedOp = vmError "E1"

inRange :: Integer -> Integer -> FN (s > TInt) (s > TBool)
inRange x y = int x # int y # opWithin

isSingleByteOp :: FN (s > TBytes) (s > TBytes > TBool)
isSingleByteOp = opSize # nat 1 # opNumEqual

isConditionalOp :: FN (s > TBytes) (s > TBool)
isConditionalOp = toSigned # int 0x63 # int 0x69 # opWithin

isOpDataOp :: FN (s > TBytes) (s > TBool)
isOpDataOp = toSigned # inRange 0x01 0x4c
