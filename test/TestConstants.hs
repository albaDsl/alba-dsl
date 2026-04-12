-- Copyright (c) 2026 albaDsl

module TestConstants (testConstants) where

import Alba.Dsl.V1.Bch2026
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude hiding (drop, reverse)

testConstants :: TestTree
testConstants =
  testGroup
    "Constants"
    [ testCase "Compile time constants" $ isTrue (evaluateProg progCompileTime),
      testCase "Runtime constants" $ isTrue (evaluateProg progRuntime)
    ]

progCompileTime :: Fn s (s > TBool)
progCompileTime =
  begin
    ∘ (c1 ∘ int 3_628_800 ∘ opNumEqualVerify)
    ∘ (c2 ∘ int 3 ∘ opNumEqualVerify)
    ∘ opTrue
  where
    c1 :: Fn s (s > TInt)
    c1 = constant (int (fac 10))

    fac :: Integer -> Integer
    fac 0 = 1
    fac n = n * fac (pred n)

    c2 :: Fn s (s > TInt)
    c2 = constant (int 2 ∘ int 3 ∘ opMul ∘ int 2 ∘ opDiv)

-- Constants that reference eachother. Tests topological ordering of runtime
-- initialization of constants.
progRuntime :: Fn s (s > TBool)
progRuntime =
  begin
    ∘ (c3 ∘ int 36 ∘ opNumEqualVerify)
    ∘ (c3 ∘ int 36 ∘ opNumEqualVerify)
    ∘ (c3 ∘ int 36 ∘ opNumEqualVerify)
    ∘ (c3 ∘ int 36 ∘ opNumEqualVerify)
    ∘ (c3 ∘ int 36 ∘ opNumEqualVerify)
    ∘ (c3 ∘ int 36 ∘ opNumEqualVerify)
    ∘ (c1 ∘ int 12 ∘ opNumEqualVerify)
    ∘ (c2 ∘ int 24 ∘ opNumEqualVerify)
    ∘ (c2 ∘ int 24 ∘ opNumEqualVerify)
    ∘ (c4 ∘ int 24 ∘ opNumEqualVerify)
    ∘ (c4 ∘ int 24 ∘ opNumEqualVerify)
    ∘ (c4 ∘ int 24 ∘ opNumEqualVerify)
    ∘ (c4 ∘ int 24 ∘ opNumEqualVerify)
    ∘ opTrue
  where
    c1 :: Fn s (s > TInt)
    c1 = runtimeConstant (staticConstant ∘ int 2 ∘ square ∘ opMul)

    c2 :: Fn s (s > TInt)
    c2 = runtimeConstant (c1 ∘ int 2 ∘ opMul)

    c3 :: Fn s (s > TInt)
    c3 = runtimeConstant (c1 ∘ c2 ∘ opAdd)

    c4 :: Fn s (s > TInt)
    c4 = runtimeConstant (c1 ∘ c1 ∘ opAdd)

    square :: Fn (s > TInt) (s > TInt)
    square = fn (opDup ∘ opMul)

    staticConstant :: Fn s (s > TInt)
    staticConstant = int 2 ∘ int 3 ∘ opMul ∘ int 2 ∘ opDiv
