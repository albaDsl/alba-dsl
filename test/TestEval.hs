-- Copyright (c) 2025 albaDsl

module TestEval (testEval) where

import Alba.Dsl.V1.BchSpec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtilsSpec (evaluateProg, isTrue)

testEval :: TestTree
testEval =
  testGroup
    "Eval"
    [ testCase "Basics" $ isTrue (evaluateProg progBasics)
    ]

progBasics :: Fn s (s > TBool)
progBasics =
  begin
    # (int 5 # progCode square # opEval square)
    # (int 25 # opNumEqualVerify)
    # opTrue
  where
    square :: Fn (s > TInt) (s > TInt)
    square = opDup # opMul
