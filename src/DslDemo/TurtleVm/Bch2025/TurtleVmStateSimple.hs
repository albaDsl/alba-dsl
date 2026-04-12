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
  ( Fn,
    FnA,
    StackEntry,
    TBytes,
    begin,
    bytes,
    cast,
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
    (∘),
    type (>),
  )
import Alba.Dsl.V1.Bch2025.Contract.Prelude (ifZero)
import DslDemo.TurtleVm.Bch2025.Maybe (TMaybe, just, nothing)

data TCode

instance StackEntry TCode

data TFunction

instance StackEntry TFunction

initState :: FnA (s > TBytes) alt s (alt > TFunction > TCode)
initState = bytes [] ∘ toFunction ∘ opToAltStack ∘ toCode ∘ opToAltStack

-- Make OP_INVOKE = op0 at startup. Should really be a VmError when invoking an
-- undefined function but that currently makes MiniTurtleVm101 too large.
initStateWithDefaultOpDefine :: FnA (s > TBytes) alt s (alt > TFunction > TCode)
initStateWithDefaultOpDefine =
  begin
    ∘ bytes [0x0]
    ∘ toFunction
    ∘ opToAltStack
    ∘ toCode
    ∘ opToAltStack

getOp ::
  FnA s (alt > TCode) (s > TMaybe TBytes) (alt > TCode)
getOp =
  begin
    ∘ (getCode ∘ opSize)
    ∘ ifZero (putCode ∘ nothing) (nat 1 ∘ opSplit ∘ putCode ∘ just)

getOpBytes ::
  Int -> FnA s (alt > TCode) (s > TBytes) (alt > TCode)
getOpBytes count = getCode ∘ (nat (fromIntegral count) ∘ opSplit ∘ putCode)

putFunction ::
  FnA (s > TBytes) (alt > TFunction > TCode) s (alt > TFunction > TCode)
putFunction =
  begin
    ∘ (opFromAltStack ∘ opFromAltStack)
    ∘ (opDrop ∘ opSwap ∘ toFunction)
    ∘ (opToAltStack ∘ opToAltStack)

getFunction ::
  FnA s (alt > TFunction > TCode) (s > TBytes) (alt > TFunction > TCode)
getFunction =
  begin
    ∘ (opFromAltStack ∘ opFromAltStack)
    ∘ (op2Dup ∘ opToAltStack ∘ opToAltStack)
    ∘ (opNip ∘ fromFunction)

invokeFunction ::
  FnA s (alt > TFunction > TCode) s (alt > TFunction > TCode)
invokeFunction =
  begin
    ∘ (opFromAltStack ∘ fromCode ∘ opFromAltStack ∘ fromFunction)
    ∘ (opDup ∘ opRot ∘ opCat ∘ opSwap)
    ∘ (toFunction ∘ opToAltStack ∘ toCode ∘ opToAltStack)

getCode :: FnA s (alt > TCode) (s > TBytes) alt
getCode = opFromAltStack ∘ fromCode

putCode :: FnA (s > TBytes) alt s (alt > TCode)
putCode = toCode ∘ opToAltStack

toCode :: Fn (s > TBytes) (s > TCode)
toCode = cast

fromCode :: Fn (s > TCode) (s > TBytes)
fromCode = cast

toFunction :: Fn (s > TBytes) (s > TFunction)
toFunction = cast

fromFunction :: Fn (s > TFunction) (s > TBytes)
fromFunction = cast
