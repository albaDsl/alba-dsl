-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmUtils
  ( vmError,
    toSigned,
    unsupportedOp,
    unsupportedOpBytes,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Bytes,
    FN,
    FNA,
    S (S),
    TBytes,
    TInt,
    bytes,
    function,
    opBin2Num,
    opCat,
    opFalse,
    opVerify,
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
