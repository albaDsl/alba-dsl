-- Copyright (c) 2025 albaDsl

module TestFunctionsLowLevel (testFunctionsLowLevel) where

import Alba.Dsl.V1.Bch2026
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude hiding (drop)

testFunctionsLowLevel :: TestTree
testFunctionsLowLevel =
  testGroup
    "Functions Low-level"
    [ testCase "Absolute" $ isTrue (evaluateProg progAbsolute),
      testCase "Named" $ isTrue (evaluateProg progNamed),
      testCase "Raw" $ isTrue (evaluateProg progRaw),
      testCase "Mixed" $ isTrue (evaluateProg progMixed)
    ]

progAbsolute :: FN s (s > TBool)
progAbsolute =
  begin
    # progCode (cube)
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
    # progCode cube
    # opDefineNamed "cube"
    # int 3
    # opInvokeNamed "cube" cube
    # int 27
    # opNumEqual

progRaw :: FN s (s > TBool)
progRaw =
  begin
    # progCode cube
    # bytes "cube"
    # opDefine
    # int 3
    # bytes "cube"
    # opInvoke cube
    # int 27
    # opNumEqual

progMixed :: FN s (s > TBool)
progMixed =
  begin
    # progCode add1
    # opDefineIdx 0
    # int 2
    # double
    # double
    # progCode cube
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
