-- Copyright (c) 2025 albaDsl

module TestFunctionsLowLevel (testFunctionsLowLevel) where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (i2SeUnsafe)
import Alba.Vm.Common.Logging (defaultDisplayOpts, dumpLog)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Prelude hiding (drop)

testFunctionsLowLevel :: TestTree
testFunctionsLowLevel =
  testGroup
    "Functions Low-level"
    [ testCase "Absolute" $ evaluateProg progAbsolute,
      testCase "Named" $ evaluateProg progNamed,
      testCase "Raw" $ evaluateProg progRaw,
      testCase "Mixed" $ evaluateProg progMixed
    ]

progAbsolute :: FN s (s > TBool)
progAbsolute =
  begin
    # bytes (compile None cube)
    # opDefineIdx 0
    # int 3
    # opInvokeIdx 0 cube
    # int 27
    # opNumEqual

cube :: FN (s > TInt) (s > TInt)
cube = opDup # opDup # opMul # opMul

progNamed :: FN s (s > TBool)
progNamed =
  begin
    # bytes (compile None cube)
    # opDefineNamed "cube"
    # int 3
    # opInvokeNamed "cube" cube
    # int 27
    # opNumEqual

progRaw :: FN s (s > TBool)
progRaw =
  begin
    # bytes (compile None cube)
    # int 0
    # opDefine
    # int 3
    # int 0
    # opInvoke cube
    # int 27
    # opNumEqual

progMixed :: FN s (s > TBool)
progMixed =
  begin
    # bytes (compile None add1)
    # opDefineIdx 0
    # int 2
    # double
    # double
    # bytes (compile None cube)
    # opDefineNamed "cube"
    # opInvokeNamed "cube" cube
    # opInvokeIdx 0 add1
    # int 4097
    # opNumEqual
  where
    double :: FN (s > TInt) (s > TInt)
    double = function (opDup # opMul)

    add1 :: FN (s > TInt) (s > TInt)
    add1 = op1Add

evaluateProg :: FNA s '[] s' alt' -> Assertion
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Right (VmState {s, alt}) -> do
          (s, alt) @?= (S.fromList [i2SeUnsafe 1], S.empty)
        Left (err, state') -> do
          dumpLog defaultDisplayOpts (fromMaybe (error "") state')
          error ("err: " <> show err)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
