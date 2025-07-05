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
      testCase "Recursion — factorial" $ evaluateProg progFactorial @?= True,
      testProperty "Recursion — merge sort" propSort
    ]

progBasic :: FN s (s > TBool)
progBasic =
  begin
    # op3
    # cube
    # int 27
    # opNumEqual
  where
    cube :: FN (s > TInt) (s > TInt)
    cube = function (opDup # opDup # opMul # opMul)

progNestedCalls :: FN s (s > TBool)
progNestedCalls =
  begin
    # int 5
    # polynomial
    # int 132
    # opNumEqual
  where
    cube :: FN (s > TInt) (s > TInt)
    cube = function (opDup # opDup # opMul # opMul)

    -- When using "S -> S" syntax, surround it in an 'F' for VM functions.
    polynomial :: F (S (s > TInt) alt -> S (s > TInt) alt)
    polynomial = function (cube # int 7 # opAdd)

progFactorial :: FN s (s > TBool)
progFactorial =
  begin
    # (nat 0 # fac # nat 1 # opNumEqual)
    # (nat 1 # fac # nat 1 # opNumEqual)
    # (nat 6 # fac # nat 720 # opNumEqual)
    # (nat 10 # fac # nat 3_628_800 # opNumEqual)
    # (opBoolAnd # opBoolAnd # opBoolAnd)
  where
    fac :: FN (s > TNat) (s > TNat)
    fac = function (unname @1 fac')

    fac' :: FN (s > N "n" TNat) (s > TNat)
    fac' =
      begin
        # argPick @"n"
        # ifZero
          (nat 1 # argsDrop @1)
          ( begin
              # argPick @"n"
              # (argRoll @"n" # op1SubUnsafe # fac)
              # opMul
          )

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 20) ==>
    evaluateProg (bytes xs # MS.sort # bytes (B.sort xs) # opEqual)

evaluateProg :: FNA s '[] s' alt' -> Bool
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Right VmState {s, alt} ->
          s == S.fromList [i2SeUnsafe 1] && alt == S.empty
        Left (err, _) -> error ("err: " <> show err)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
