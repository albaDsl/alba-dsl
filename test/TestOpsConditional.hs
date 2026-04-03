-- Copyright (c) 2025 albaDsl

module TestOpsConditional (testOpsConditional) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common (ScriptError (SeOpReturn, SeVerify))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils (evaluateProg, isErr, isTrue)

testOpsConditional :: TestTree
testOpsConditional =
  testGroup
    "Conditionals"
    [ testCase "opIf" $ isTrue (evaluateProg progIf),
      testCase "opNotIf" $ isTrue (evaluateProg progNotIf),
      testCase "Nested If" $ isTrue (evaluateProg progNestedIf),
      testCase "OpVerify" $ isErr (evaluateProg progVerify) SeVerify,
      testCase "OpReturn" $ isErr (evaluateProg progReturn) SeOpReturn
    ]

progIf :: Fn s (s > TBool)
progIf =
  begin
    # opFalse
    # opIf op1 addUp
    # int 3
    # opNumEqual
  where
    addUp = op1 # op2 # opAdd

progNotIf :: Fn s (s > TBool)
progNotIf =
  begin
    # opFalse
    # opNotIf op1 addUp
    # int 1
    # opNumEqual
  where
    addUp = op1 # op2 # opAdd

progNestedIf :: Fn s (s > TBool)
progNestedIf =
  begin
    # opTrue
    # opIf
      ( begin
          # opFalse
          # opIf
            (op1 # op2 # opAdd)
            (op1 # op3 # opAdd)
      )
      (op1 # op1 # opAdd)
    # int 4
    # opNumEqual

progVerify :: FnC
progVerify = opFalse # opVerify

progReturn :: Fn s (s > TBytes)
progReturn = opReturn # bytes "hello world!"
