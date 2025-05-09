-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestOpsConditional (testOpsConditional) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common (ScriptError (SeOpReturn, SeVerify))
import Alba.Vm.Common.StackElement
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProg)

testOpsConditional :: TestTree
testOpsConditional =
  testGroup
    "Conditionals"
    [ testCase "opIf" $
        let Right (s, alt) = evaluateProg progIf
         in (s, alt) @?= (S.singleton $ i2SeUnsafe 3, S.empty),
      testCase "opNotIf" $
        let Right (s, alt) = evaluateProg progNotIf
         in (s, alt) @?= (S.singleton $ i2SeUnsafe 1, S.empty),
      testCase "Nested If" $
        let Right (s, alt) = evaluateProg progNestedIf
         in (s, alt) @?= (S.singleton $ i2SeUnsafe 4, S.empty),
      testCase "OpVerify" $
        let res = evaluateProg progVerify
         in res @?= Left SeVerify,
      testCase "OpReturn" $
        let res = evaluateProg progReturn
         in Left SeOpReturn @?= res
    ]

progIf :: FN s (s > TNat)
progIf =
  begin
    # opFalse
    # opIf op1 addUp
  where
    addUp = op1 # op2 # opAdd

progNotIf :: FN s (s > TNat)
progNotIf =
  begin
    # opFalse
    # opNotIf op1 addUp
  where
    addUp = op1 # op2 # opAdd

progNestedIf :: FN s (s > TNat)
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

progVerify :: FNC
progVerify = opFalse # opVerify

progReturn :: FN s (s > TBytes)
progReturn = opReturn # bytes "hello world!"
