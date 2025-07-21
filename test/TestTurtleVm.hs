-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestTurtleVm (testTurtleVm) where

import Alba.Dsl.V1.Bch2025
import Alba.Dsl.V1.Common.StackUntyped (FNU, toTyped)
import Alba.Vm.Bch2025
  ( ScriptError,
    VmParams (..),
    VmState (..),
    b2SeUnsafe,
    i2SeUnsafe,
  )
import Alba.Vm.Bch2025 qualified as Bch2025
import Alba.Vm.Common.Logging (defaultDisplayOpts, dumpLog)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import DslDemo.TurtleVm.MiniTurtleVm101
  ( miniTurtleVm101,
    turtleOpDefine,
    turtleOpInvoke,
  )
import DslDemo.TurtleVm.TurtleVm (turtleVm)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import TestUtils (txContext, utxoWithPubkey)
import Prelude hiding (drop)

testTurtleVm :: TestTree
testTurtleVm =
  testGroup
    "turtleVm"
    [ testCase "Data pushing" $
        expectTrueResult (evaluateOnTurtleVm progDataPush),
      testCase "Conditionals" $
        expectTrueResult (evaluateOnTurtleVm progConditionals),
      testCase "Stack — 1" $
        expectTrueResult (evaluateOnTurtleVm progStack1),
      testCase "Stack — 2" $
        expectTrueResult (evaluateOnTurtleVm progStack2),
      testCase "Bytes" $
        expectTrueResult (evaluateOnTurtleVm progBytes),
      testCase "Bitwise" $
        expectTrueResult (evaluateOnTurtleVm progBitwise),
      testCase "Arithmetic — 1" $
        expectTrueResult (evaluateOnTurtleVm progArithmetic1),
      testCase "Arithmetic — 2" $
        expectTrueResult (evaluateOnTurtleVm progArithmetic2),
      testCase "Introspection" $
        expectTrueResult (evaluateOnTurtleVm progIntrospection),
      testCase "MiniTurtle" $
        case evaluateOnMiniTurtleVm progMiniTurtle of
          Right state -> do
            -- dumpLog defaultDisplayOpts state
            state.s @?= S.singleton (i2SeUnsafe 1)
          Left (err, state) -> do
            dumpLog defaultDisplayOpts (fromMaybe (error "") state)
            assertFailure (show err)
    ]

progDataPush :: FN s (s > TBool)
progDataPush =
  begin
    # (bytes [1, 2, 3] # bytes [1, 2] # bytes [3] # opCat # opEqual)
    # (bytes (B.pack $ replicate 74 1) # nat 37 # opSplit # opEqual)
    # opBoolAnd

progConditionals :: FN s (s > TBool)
progConditionals =
  begin
    # opTrue
    # opIf
      ( begin
          # opTrue
          # opNotIf
            opFalse
            ( begin
                # opFalse
                # opIf opFalse opTrue
            )
      )
      opFalse

progArithmetic1 :: FN s (s > TBool)
progArithmetic1 =
  begin
    # (int 2 # int 3 # opMul # int 4 # opAdd # int 2 # opDiv)
    # (int 7 # int 5 # opMod)
    # opSub
    # (int 3 # opNumEqual)

progArithmetic2 :: FN s (s > TBool)
progArithmetic2 =
  begin
    # (int 2 # int 3 # opLessThan)
    # (int 3 # int 3 # opLessThanOrEqual)
    # (int 3 # int 4 # opNumNotEqual)
    # (int 3 # int 1 # int 5 # opWithin)
    # (opBoolAnd # opBoolAnd # opBoolAnd)

progIntrospection :: FN s (s > TBool)
progIntrospection =
  begin
    # (opTxVersion # nat 2 # opNumEqual)
    # (opTxInputCount # nat 1 # opNumEqual)
    # opBoolAnd

progStack1 :: FN s (s > TBool)
progStack1 = nat 2 # opTrue # opNip # opDup # opDrop

progStack2 :: FN s (s > TBool)
progStack2 =
  begin
    # name @"x4" (int 4)
    # name @"x3" (int 3)
    # name @"x2" (int 2)
    # name @"x1" (int 1)
    # name @"x0" (int 0)
    # (pick @"x4" # roll @"x3" # opMul # int 12 # opNumEqual)
    # (drop @"x0" # drop @"x1" # drop @"x2" # drop @"x4")

progBytes :: FN s (s > TBool)
progBytes =
  begin
    # startBytes -- b
    # opSize -- b s
    # opSwap -- s b
    # opDup -- s b b
    # opReverseBytes -- s b br
    # opCat -- s b(br)
    # opSwap -- b(br) s
    # opSplit -- b br
    # opReverseBytes -- b b
    # opEqual -- t
  where
    startBytes :: FN s (s > TBytes)
    startBytes = int 1 # toBytes # int 2 # toBytes # opCat

    toBytes :: FN (s > TInt) (s > TBytes)
    toBytes = cast

progBitwise :: FN s (s > TBool)
progBitwise =
  int 1 # toBytes # int 2 # toBytes # opOr # int 3 # toBytes # opEqual
  where
    toBytes :: FN (s > TInt) (s > TBytes)
    toBytes = cast

progMiniTurtle :: FN s (s > TInt)
progMiniTurtle =
  begin
    # bytes (compile None f)
    # turtleOpDefine
    # int 1
    # turtleOpInvoke f
  where
    f = int 1 # opMul

evaluateOnTurtleVm ::
  FNA s '[] s' alt' ->
  Either (ScriptError, Maybe VmState) VmState
evaluateOnTurtleVm =
  evaluate (turtleVm maxOps maxCondStackDepth) largerLimits
  where
    maxOps = 20

    maxCondStackDepth = 5

    largerLimits :: VmParams -> VmParams
    largerLimits params =
      params
        { maxTxInScriptSigSize = Just 40_000,
          maxScriptSize = 40_000
        }

evaluateOnMiniTurtleVm ::
  FNA s '[] s' alt' ->
  Either (ScriptError, Maybe VmState) VmState
evaluateOnMiniTurtleVm = evaluate miniTurtleVm101 id

{-# INLINE evaluate #-}
evaluate ::
  FNU ->
  (VmParams -> VmParams) ->
  FNA s '[] s' alt' ->
  Either (ScriptError, Maybe VmState) VmState
evaluate vm updateParams prog =
  let prog' = compile None prog
      vmCode = compile None (toTyped vm)
      state =
        (Bch2025.startState (updateParams Bch2025.vmParamsStandard))
          { Bch2025.code = vmCode,
            Bch2025.s = S.singleton $ b2SeUnsafe prog',
            Bch2025.alt = S.empty
          }
      ctx = txContext (utxoWithPubkey vmCode)
   in Bch2025.evaluateScript ctx state

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
