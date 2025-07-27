-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVmUtils (vmError, toSigned) where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    FN,
    FNA,
    S (S),
    TBytes,
    TInt,
    bytes,
    opBin2Num,
    opCat,
    opFalse,
    opVerify,
    (#),
    type (>),
  )

vmError :: Bytes -> FNA s alt s' alt'
vmError msg = bytes msg # opFalse # opVerify # castStack

castStack :: FNA s alt s' alt'
castStack (S c fs) = let state = S c fs in state

toSigned :: FN (s > TBytes) (s > TInt)
toSigned = bytes [0] # opCat # opBin2Num
