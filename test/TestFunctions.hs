-- Copyright (c) 2025 albaDsl

module TestFunctions (testFunctions) where

import Alba.Dsl.V1.Bch2026
import Data.ByteString qualified as B
import DslDemo.MergeSort.MergeSort qualified as MS
import QuickCheckSupport (AsciiString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (drop)

testFunctions :: TestTree
testFunctions =
  testGroup
    "Functions"
    [ testCase "Basic function call" $ isTrue (evaluateProg progBasic),
      testCase "Nested function calls" $ isTrue (evaluateProg progNestedCalls),
      testCase "Recursion - factorial" $ isTrue (evaluateProg progFactorial),
      testProperty "Recursion - merge sort" propSort
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
        # pick @"n"
        # ifZero
          (nat 1 # drop @"n")
          (pick @"n" # roll @"n" # op1SubUnsafe # fac # opMul)

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 48) ==> do
    isTrue' $ evaluateProg (bytes xs # MS.sort # bytes (B.sort xs) # opEqual)
