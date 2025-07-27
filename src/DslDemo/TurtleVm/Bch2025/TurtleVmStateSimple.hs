-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVmStateSimple
  ( initState,
    initStateWithDefaultOpDefine,
    getOp,
    getOpBytes,
    putFunction,
    getFunction,
    invokeFunction,
  )
where

import Alba.Dsl.V1.Bch2025
  ( FN,
    FNA,
    StackEntry,
    TBytes,
    begin,
    bytes,
    cast,
    ifZero,
    nat,
    op2Dup,
    opCat,
    opDrop,
    opDup,
    opFromAltStack,
    opNip,
    opRot,
    opSize,
    opSplit,
    opSwap,
    opToAltStack,
    (#),
    type (>),
  )
import DslDemo.TurtleVm.Common.Maybe (TMaybe, just, nothing)

data TCode

instance StackEntry TCode

data TFunction

instance StackEntry TFunction

initState :: FNA (s > TBytes) alt s (alt > TFunction > TCode)
initState = bytes [] # toFunction # opToAltStack # toCode # opToAltStack

-- Make OP_INVOKE = op0 at startup. Should really be a VmError when invoking an
-- undefined function but that currently makes MiniTurtleVm101 too large.
initStateWithDefaultOpDefine :: FNA (s > TBytes) alt s (alt > TFunction > TCode)
initStateWithDefaultOpDefine =
  begin
    # bytes [0x0]
    # toFunction
    # opToAltStack
    # toCode
    # opToAltStack

getOp ::
  FNA s (alt > TCode) (s > TMaybe TBytes) (alt > TCode)
getOp =
  begin
    # (getCode # opSize)
    # ifZero (putCode # nothing) (nat 1 # opSplit # putCode # just)

getOpBytes ::
  Int -> FNA s (alt > TCode) (s > TBytes) (alt > TCode)
getOpBytes count = getCode # (nat (fromIntegral count) # opSplit # putCode)

putFunction ::
  FNA (s > TBytes) (alt > TFunction > TCode) s (alt > TFunction > TCode)
putFunction =
  begin
    # (opFromAltStack # opFromAltStack)
    # (opDrop # opSwap # toFunction)
    # (opToAltStack # opToAltStack)

getFunction ::
  FNA s (alt > TFunction > TCode) (s > TBytes) (alt > TFunction > TCode)
getFunction =
  begin
    # (opFromAltStack # opFromAltStack)
    # (op2Dup # opToAltStack # opToAltStack)
    # (opNip # fromFunction)

invokeFunction ::
  FNA s (alt > TFunction > TCode) s (alt > TFunction > TCode)
invokeFunction =
  begin
    # (opFromAltStack # fromCode # opFromAltStack # fromFunction)
    # (opDup # opRot # opCat # opSwap)
    # (toFunction # opToAltStack # toCode # opToAltStack)

getCode :: FNA s (alt > TCode) (s > TBytes) alt
getCode = opFromAltStack # fromCode

putCode :: FNA (s > TBytes) alt s (alt > TCode)
putCode = toCode # opToAltStack

toCode :: FN (s > TBytes) (s > TCode)
toCode = cast

fromCode :: FN (s > TCode) (s > TBytes)
fromCode = cast

toFunction :: FN (s > TBytes) (s > TFunction)
toFunction = cast

fromFunction :: FN (s > TFunction) (s > TBytes)
fromFunction = cast
