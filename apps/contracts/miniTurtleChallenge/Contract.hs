-- Copyright (c) 2025 albaDsl

module Contract (MiniTurtleChallenge, contract) where

import Alba.Dsl.V1.Bch2026
  ( CFn,
    Contract (..),
    Fn,
    N,
    Stack (..),
    TBool,
    TBytes,
    begin,
    bytes,
    int,
    name2,
    nat,
    opDup,
    opEqual,
    opGreaterThanOrEqual,
    opIf,
    opLessThanOrEqual,
    opNip,
    opNot,
    opNumEqualVerify,
    opSize,
    opSplit,
    opSwap,
    opTrue,
    opVerify,
    opWhen,
    pick,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026.LangUntyped (repeatProg)
import Alba.Dsl.V1.Bch2026.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped, toTyped, (∘))
import DslDemo.TurtleVm.Bch2025.MiniTurtleVm101 (miniTurtleVm101)
import Prelude hiding (drop, (.))

type MiniTurtleChallenge =
  Contract
    "MiniTurtleChallenge"
    (Base :> TBytes)
    '["withdraw"]
    Base

contract :: MiniTurtleChallenge
contract = MkContract withdraw

withdraw :: CFn (Base :> TBytes)
withdraw =
  begin
    . verifyProgramSize
    . verifyBytecode
    . (toTyped miniTurtleVm101 . int 5 . opNumEqualVerify)
    . opTrue

verifyProgramSize :: Fn (s :> TBytes) (s :> TBytes)
verifyProgramSize =
  opSize . nat (fromIntegral progMaxSize) . opLessThanOrEqual . opVerify

-- The same opcode/byte can not appear twice in a row. OP_1ADD may not follow
-- OP_MUL.
verifyBytecode :: Fn (s :> TBytes) (s :> TBytes)
verifyBytecode = opDup . bytes [255] . opSwap . toTyped checkAll
  where
    checkAll :: FnU
    checkAll = repeatProg progMaxSize (fromTyped check) ∘ UT.op2Drop

    check ::
      Fn
        (s :> N "lastOp" TBytes :> N "ops" TBytes)
        (s :> TBytes :> TBytes)
    check =
      begin
        . (pick #ops . isNotEmpty)
        . opIf
          ( begin
              . name2 #op #ops' (roll #ops . nat 1 . opSplit)
              . (pick #lastOp . pick #op . verifyNotEqual)
              . ( begin
                    . (roll #lastOp . mul . opEqual)
                    . opWhen (pick #op . add1 . verifyNotEqual)
                )
              . (roll #op . roll #ops')
          )
          (roll #lastOp . roll #ops)

    mul = bytes [0x95]

    add1 = bytes [0x8B]

    verifyNotEqual :: Fn (s :> TBytes :> TBytes) s
    verifyNotEqual = opEqual . opNot . opVerify

    isNotEmpty :: Fn (s :> TBytes) (s :> TBool)
    isNotEmpty = opSize . nat 1 . opGreaterThanOrEqual . opNip

progMaxSize :: Int
progMaxSize = 9
