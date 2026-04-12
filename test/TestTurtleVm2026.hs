-- Copyright (c) 2025 albaDsl

module TestTurtleVm2026 (testTurtleVm2026) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.StackUntyped (FnU, toTyped)
import Alba.Vm.Bch2026
  ( ScriptError (SeVerify),
    VmParams (..),
    VmState (..),
    b2SeUnsafe,
    i2SeUnsafe,
  )
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.Common.Logging (defaultDisplayOpts)
import Alba.Vm.Common.LoggingText (dumpLog)
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (OP_RESERVED))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import DslDemo.TurtleVm.Bch2026.TurtleVm (turtleVm)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import TestUtils (txContext, utxoWithPubkey)

testTurtleVm2026 :: TestTree
testTurtleVm2026 =
  testGroup
    "turtleVm 2026"
    [ testCase "Data pushing" $
        expectTrueResult (evaluateOnTurtleVm progDataPush),
      testCase "Conditionals" $
        expectTrueResult (evaluateOnTurtleVm progConditionals),
      testCase "Stack - 1" $
        expectTrueResult (evaluateOnTurtleVm progStack1),
      testCase "Stack - 2" $
        expectTrueResult (evaluateOnTurtleVm progStack2),
      testCase "Alt Stack" $
        expectTrueResult (evaluateOnTurtleVm progAltStack),
      testCase "Bytes" $
        expectTrueResult (evaluateOnTurtleVm progBytes),
      testCase "Bitwise" $
        expectTrueResult (evaluateOnTurtleVm progBitwise),
      testCase "Arithmetic - 1" $
        expectTrueResult (evaluateOnTurtleVm progArithmetic1),
      testCase "Arithmetic - 2" $
        expectTrueResult (evaluateOnTurtleVm progArithmetic2),
      testCase "Introspection" $
        expectTrueResult (evaluateOnTurtleVm progIntrospection),
      testCase "OP_RESERVED" $
        expectVmError (evaluateOnTurtleVm progOpReserved),
      testCase "OP_ACTIVEBYTECODE" $
        expectVmError (evaluateOnTurtleVm progOpActiveBytecode)
    ]

progDataPush :: Fn s (s > TBool)
progDataPush =
  begin
    ∘ (bytes [1, 2, 3] ∘ bytes [1, 2] ∘ bytes [3] ∘ opCat ∘ opEqual)
    ∘ (bytes (B.pack $ replicate 74 1) ∘ nat 37 ∘ opSplit ∘ opEqual)
    ∘ opBoolAnd

-- Use multi-byte strings inside conditionals. If multi-byte opcodes are not
-- handled correctly by the VM, then it could start interpreting the data as
-- opcodes.
progConditionals :: Fn s (s > TBool)
progConditionals =
  begin
    ∘ opTrue
    ∘ opIf
      ( begin
          ∘ opTrue
          ∘ opNotIf
            someBytes
            ( begin
                ∘ opFalse
                ∘ opIf someBytes expectedBytes
            )
      )
      someBytes
    ∘ expectedBytes
    ∘ opEqual
  where
    expectedBytes :: Fn s (s > TBytes)
    expectedBytes = bytes [opEndif, opEndif, opEndif]

    someBytes :: Fn s (s > TBytes)
    someBytes = bytes [opElse, opElse, opElse]

    opElse = 0x67

    opEndif = 0x68

progArithmetic1 :: Fn s (s > TBool)
progArithmetic1 =
  begin
    ∘ (int 2 ∘ int 3 ∘ opMul ∘ int 4 ∘ opAdd ∘ int 2 ∘ opDiv)
    ∘ (int 7 ∘ int 5 ∘ opMod)
    ∘ opSub
    ∘ (int 3 ∘ opNumEqual)

progArithmetic2 :: Fn s (s > TBool)
progArithmetic2 =
  begin
    ∘ (int 2 ∘ int 3 ∘ opLessThan)
    ∘ (int 3 ∘ int 3 ∘ opLessThanOrEqual)
    ∘ (int 3 ∘ int 4 ∘ opNumNotEqual)
    ∘ (int 3 ∘ int 1 ∘ int 5 ∘ opWithin)
    ∘ (opBoolAnd ∘ opBoolAnd ∘ opBoolAnd)

progIntrospection :: Fn s (s > TBool)
progIntrospection =
  begin
    ∘ (opTxVersion ∘ nat 2 ∘ opNumEqual)
    ∘ (opTxInputCount ∘ nat 1 ∘ opNumEqual)
    ∘ opBoolAnd

progStack1 :: Fn s (s > TBool)
progStack1 = nat 2 ∘ opTrue ∘ opNip ∘ opDup ∘ opDrop

progStack2 :: Fn s (s > TBool)
progStack2 =
  begin
    ∘ name #x4 (int 4)
    ∘ name #x3 (int 3)
    ∘ name #x2 (int 2)
    ∘ name #x1 (int 1)
    ∘ name #x0 (int 0)
    ∘ (pick #x4 ∘ roll #x3 ∘ opMul ∘ int 12 ∘ opNumEqual)
    ∘ (del #x0 ∘ del #x1 ∘ del #x2 ∘ del #x4)

progAltStack :: Fn s (s > TBool)
progAltStack =
  begin
    ∘ int 5
    ∘ int 3
    ∘ int 7
    ∘ opToAltStack
    ∘ opToAltStack
    ∘ opToAltStack
    ∘ opFromAltStack
    ∘ opFromAltStack
    ∘ opSub
    ∘ opFromAltStack
    ∘ opMul
    ∘ int 14
    ∘ opNumEqual

progBytes :: Fn s (s > TBool)
progBytes =
  begin
    ∘ startBytes -- b
    ∘ opSize -- b s
    ∘ opSwap -- s b
    ∘ opDup -- s b b
    ∘ opReverseBytes -- s b br
    ∘ opCat -- s b(br)
    ∘ opSwap -- b(br) s
    ∘ opSplit -- b br
    ∘ opReverseBytes -- b b
    ∘ opEqual -- t
  where
    startBytes :: Fn s (s > TBytes)
    startBytes = int 1 ∘ i2b ∘ int 2 ∘ i2b ∘ opCat

i2b :: Fn (s > TInt) (s > TBytes)
i2b = cast

progBitwise :: Fn s (s > TBool)
progBitwise =
  int 1 ∘ i2b ∘ int 2 ∘ i2b ∘ opOr ∘ int 3 ∘ i2b ∘ opEqual

progOpReserved :: Fn s s
progOpReserved = insertOpCode (OP_UNUSED OP_RESERVED)

progOpActiveBytecode :: Fn s s
progOpActiveBytecode = insertOpCode OP_ACTIVEBYTECODE

insertOpCode :: OpcodeL2 -> Fn s s
insertOpCode op = aop op

evaluateOnTurtleVm ::
  FnA s '[] s' alt' ->
  Either (ScriptError, Maybe VmState) VmState
evaluateOnTurtleVm =
  evaluate (turtleVm maxCondStackDepth) largerLimits
  where
    maxCondStackDepth = 5

    largerLimits :: VmParams -> VmParams
    largerLimits params =
      params
        { maxTxInScriptSigSize = Just 40_000,
          maxScriptSize = 40_000
        }

{-# INLINE evaluate #-}
evaluate ::
  FnU ->
  (VmParams -> VmParams) ->
  FnA s '[] s' alt' ->
  Either (ScriptError, Maybe VmState) VmState
evaluate vm updateParams prog =
  let prog' = compile None prog
      vmCode = compile None (toTyped vm)
      state =
        (Bch2026.startState (updateParams Bch2026.vmParamsStandard))
          { Bch2026.code = vmCode,
            Bch2026.s = S.singleton $ b2SeUnsafe prog',
            Bch2026.alt = S.empty
          }
      ctx = txContext (utxoWithPubkey vmCode)
   in Bch2026.evaluateScript ctx state

expectTrueResult :: Either (ScriptError, Maybe VmState) VmState -> Assertion
expectTrueResult result =
  case result of
    Right state -> do
      -- dumpLog defaultDisplayOpts state
      (state.s, state.alt)
        @?= (S.singleton $ i2SeUnsafe 1, S.singleton emptyTuple)
    Left (err, state) -> do
      dumpLog defaultDisplayOpts (fromMaybe (error "") state)
      assertFailure (show err)
  where
    emptyTuple = b2SeUnsafe [0, 0]

expectVmError :: Either (ScriptError, Maybe VmState) VmState -> Assertion
expectVmError result =
  case result of
    Right state -> do
      dumpLog defaultDisplayOpts state
      assertFailure "Exepected a TurtleVm vmError."
    Left (err, _state) -> do
      err @?= SeVerify
