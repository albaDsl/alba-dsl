-- Copyright (c) 2025 albaDsl

module TestFunctions (testFunctions) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Int64 (toInt64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8)
import Alba.Dsl.V1.Bch2026.Contract.Vector (TVector, generate, reverse)
import Data.ByteString qualified as B
import DslDemo.EllipticCurve.Field (feAdd, feCube, feMul, feSub)
import DslDemo.MergeSort.MergeSort qualified as MS
import QuickCheckSupport (AsciiString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (reverse)

testFunctions :: TestTree
testFunctions =
  testGroup
    "Functions"
    [ testCase "Basic function call" $ isTrue (evaluateProg progBasic),
      testCase "Nested function calls - 1" $
        isTrue (evaluateProg progNestedCalls1),
      testCase "Nested function calls - 2" $
        isTrue (evaluateProg progNestedCalls2),
      testCase "Recursion - factorial" $ isTrue (evaluateProg progFactorial),
      testCase "Recursion - merge sort TInt64s" $
        isTrue (evaluateProg progSort),
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

-- Evaluate: polynomial(x) = x⁴ + 10x³ + 35x² + 50x + 24 for
-- x = 1, 2, 3, and 4.
progNestedCalls1 :: FN s (s > TBool)
progNestedCalls1 =
  begin
    # (int 2 # polynomial # int 360 # opNumEqualVerify)
    # (int 3 # polynomial # int 840 # opNumEqualVerify)
    # (int 4 # polynomial # int 1680 # opNumEqualVerify)
    # (int 5 # polynomial # int 3024 # opNumEqualVerify)
    # opTrue
  where
    polynomial :: F (S (s > TInt) alt -> S (s > TInt) alt)
    polynomial = function (unname @1 polynomial')

    -- When using "S -> S" syntax, surround it in an 'F' for VM functions.
    polynomial' :: F (S (s > N "x" TInt) alt -> S (s > TInt) alt)
    polynomial' =
      begin
        # (pick @"x" # quartic)
        # (pick @"x" # cube # int 10 # opMul)
        # (pick @"x" # square # int 35 # opMul)
        # (roll @"x" # int 50 # opMul)
        # int 24
        # (opAdd # opAdd # opAdd # opAdd)

    quartic :: FN (s > TInt) (s > TInt)
    quartic = function (square # square)

    cube :: FN (s > TInt) (s > TInt)
    cube = function (opDup # opDup # opMul # opMul)

    square :: FN (s > TInt) (s > TInt)
    square = function (opDup # opMul)

progNestedCalls2 :: FN s (s > TBool)
progNestedCalls2 =
  begin
    # (int 3 # int 7 # feMul # int 1 # feAdd # int 12 # feSub # feCube)
    # (int 1000 # opNumEqual)

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
          (nat 1 # del @"n")
          (pick @"n" # roll @"n" # op1SubUnsafe # fac # opMul)

progSort :: FN s (s > TBool)
progSort =
  begin
    # (nat 10 # lambda1 (op1Add # cast # toInt64) # generate)
    # (opDup # reverse # MS.sort # opEqual)

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 47) ==> do
    isTrue' $
      evaluateProg
        (bytes xs # toVector # MS.sort # bytes (B.sort xs) # toVector # opEqual)
  where
    toVector :: FN (s > TBytes) (s > TVector TInt8)
    toVector = cast
