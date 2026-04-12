-- Copyright (c) 2025 albaDsl

module TestFunctions (testFunctions) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.BlobEq (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.PartialApplication (apply2, apply3_2)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (swap)
import Alba.Dsl.V1.Bch2026.Contract.TInt64 ()
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8)
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector, generate, reverse)
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
      testProperty "Recursion - merge sort" propSort,
      testCase "Runtime function definitions" $
        isTrue (evaluateProg progRuntimeFunctions)
    ]

progBasic :: Fn s (s > TBool)
progBasic =
  begin
    # op3
    # cube
    # int 27
    # opNumEqual
  where
    cube :: Fn (s > TInt) (s > TInt)
    cube = fn (opDup # opDup # opMul # opMul)

-- Evaluate: polynomial(x) = x⁴ + 10x³ + 35x² + 50x + 24 for
-- x = 1, 2, 3, and 4.
progNestedCalls1 :: Fn s (s > TBool)
progNestedCalls1 =
  begin
    # (int 2 # polynomial # int 360 # opNumEqualVerify)
    # (int 3 # polynomial # int 840 # opNumEqualVerify)
    # (int 4 # polynomial # int 1680 # opNumEqualVerify)
    # (int 5 # polynomial # int 3024 # opNumEqualVerify)
    # opTrue
  where
    polynomial :: F (S (s > TInt) alt -> S (s > TInt) alt)
    polynomial = fn (unname 1 polynomial')

    -- When using "S -> S" syntax, surround it in an 'F' for VM functions.
    polynomial' :: F (S (s > N "x" TInt) alt -> S (s > TInt) alt)
    polynomial' =
      begin
        # (pick "x" # quartic)
        # (pick "x" # cube # int 10 # opMul)
        # (pick "x" # square # int 35 # opMul)
        # (roll "x" # int 50 # opMul)
        # int 24
        # (opAdd # opAdd # opAdd # opAdd)

    quartic :: Fn (s > TInt) (s > TInt)
    quartic = fn (square # square)

    cube :: Fn (s > TInt) (s > TInt)
    cube = fn (opDup # opDup # opMul # opMul)

    square :: Fn (s > TInt) (s > TInt)
    square = fn (opDup # opMul)

progNestedCalls2 :: Fn s (s > TBool)
progNestedCalls2 =
  begin
    # (int 3 # int 7 # feMul # int 1 # feAdd # int 12 # feSub # feCube)
    # (int 1000 # opNumEqual)

progFactorial :: Fn s (s > TBool)
progFactorial =
  begin
    # (nat 0 # fac # nat 1 # opNumEqual)
    # (nat 1 # fac # nat 1 # opNumEqual)
    # (nat 6 # fac # nat 720 # opNumEqual)
    # (nat 10 # fac # nat 3_628_800 # opNumEqual)
    # (opBoolAnd # opBoolAnd # opBoolAnd)
  where
    fac :: Fn (s > TNat) (s > TNat)
    fac = fn (unname 1 fac')

    fac' :: Fn (s > N "n" TNat) (s > TNat)
    fac' =
      begin
        # pick "n"
        # ifZero
          (nat 1 # del "n")
          (pick "n" # roll "n" # op1SubUnsafe # fac # opMul)

progSort :: Fn s (s > TBool)
progSort =
  runEnv
    ( begin
        # (nat 10 # lambda1 (op1Add # toInt8) # generate)
        # (opDup # reverse # MS.sort # equal)
    )
  where
    toInt8 :: Fn (s > TNat) (s > TInt8)
    toInt8 = n2i # fromInt

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 47) ==> do
    isTrue' $
      evaluateProg
        (bytes xs # toVector # MS.sort # bytes (B.sort xs) # toVector # equal)
  where
    toVector :: Fn (s > TBytes) (s > TVector TInt8)
    toVector = cast

-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize progRuntimeFunctions
-- "37 opcodes, 37 bytes. Including function table: 55 opcodes, 196 bytes.\n"
progRuntimeFunctions :: FnA s Base (s > TBool) Base
progRuntimeFunctions =
  runEnv
    ( begin
        # (int 1 # int 2 # opAdd # f # apply2)
        # (int 5 # swap # invoke1 # int 2 # opNumEqualVerify)
        # (int 3 # int 2 # opAdd # f # apply2)
        # (int 9 # swap # invoke1 # int 4 # opNumEqualVerify)
        # (int 3 # int 6 # g # apply3_2)
        # (int 4 # swap # invoke1 # opTrue # opEqualVerify)
        # opTrue
    )
  where
    f = lambda2 opSub
    g = lambda3 opWithin
