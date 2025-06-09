-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestFunctions (testFunctions) where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (i2SeUnsafe)
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import DslDemo.MergeSort.MergeSort qualified as MS
import QuickCheckSupport (AsciiString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (==>))

testFunctions :: TestTree
testFunctions =
  testGroup
    "Functions"
    [ testCase "Basic function call" $ evaluateProg progBasic @?= True,
      testCase "Nested function calls" $ evaluateProg progNestedCalls @?= True,
      testCase "Nested function definitions" $
        evaluateProg progNestedDefinitions @?= True,
      testCase "Recursion — factorial" $ evaluateProg progFactorial @?= True,
      testProperty "Recursion — merge sort" propSort
    ]

progBasic :: FN s (s > TBool)
progBasic =
  begin
    # function "cube" cube
    # op3
    # invoke "cube" cube
    # int 27
    # opNumEqual
  where
    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul

progNestedCalls :: FN s (s > TBool)
progNestedCalls =
  begin
    # function "cube" cube
    # function "polynomial" polynomial
    # int 5
    # invoke "polynomial" polynomial
    # int 132
    # opNumEqual
  where
    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul

    polynomial :: S (s > TInt) alt -> S (s > TInt) alt
    polynomial = invoke "cube" cube # int 7 # opAdd

progNestedDefinitions :: FN s (s > TBool)
progNestedDefinitions =
  begin
    # function "setup" setup
    # invoke "setup" setup
    # int 5
    # invoke "polynomial" polynomial
    # int 132
    # opNumEqual
  where
    setup :: FN s s
    setup =
      begin
        # function "cube" cube
        # function "polynomial" polynomial

    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul

    polynomial :: S (s > TInt) alt -> S (s > TInt) alt
    polynomial = invoke "cube" cube # int 7 # opAdd

progFactorial :: FN s (s > TBool)
progFactorial =
  begin
    # function "fac" fac
    # (nat 0 # invoke "fac" fac # nat 1 # opNumEqual)
    # (nat 1 # invoke "fac" fac # nat 1 # opNumEqual)
    # (nat 6 # invoke "fac" fac # nat 720 # opNumEqual)
    # (nat 10 # invoke "fac" fac # nat 3_628_800 # opNumEqual)
    # (opBoolAnd # opBoolAnd # opBoolAnd)
  where
    fac :: FN (s > TNat) (s > TNat)
    fac = unname @1 fac'

    fac' :: FN (s > N "n" TNat) (s > TNat)
    fac' =
      begin
        # argPick @"n"
        # ifZero
          (nat 1 # argsDrop @1)
          ( begin
              # argPick @"n"
              # (argRoll @"n" # op1SubUnsafe # invoke "fac" fac)
              # opMul
          )

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 20) ==>
    evaluateProg (MS.setup # bytes xs # MS.sort # bytes (B.sort xs) # opEqual)

evaluateProg :: FNA s '[] s' alt' -> Bool
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Right VmState {s, alt} ->
          s == S.fromList [i2SeUnsafe 1] && alt == S.empty
        Left (err, _) -> error ("err: " <> show err)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
