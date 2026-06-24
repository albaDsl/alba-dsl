-- Copyright (c) 2025 albaDsl

module TestFunctions (testFunctions) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    Integral (..),
    TInt64,
    TInt8,
    apply2,
    apply3_2,
    ifZero,
    int64,
    nat1SubUnsafe,
    swap,
  )
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector, generate, reverse)
import Data.ByteString qualified as B
import DslDemo.EllipticCurve.Field (feAdd, feCube, feMul, feSub, pushFe)
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

progBasic :: Fn s (s :> TBool)
progBasic = op3 ∘ cube ∘ int 27 ∘ opNumEqual
  where
    cube :: Fn (s :> TInt) (s :> TInt)
    cube = fn (opDup ∘ opDup ∘ opMul ∘ opMul)

-- Evaluate: polynomial(x) = x⁴ + 10x³ + 35x² + 50x + 24 for
-- different x values.
progNestedCalls1 :: Fn s (s :> TBool)
progNestedCalls1 =
  begin
    ∘ (int 2 ∘ polynomial ∘ int 360 ∘ opNumEqualVerify)
    ∘ (int 3 ∘ polynomial ∘ int 840 ∘ opNumEqualVerify)
    ∘ (int 4 ∘ polynomial ∘ int 1680 ∘ opNumEqualVerify)
    ∘ (int 5 ∘ polynomial ∘ int 3024 ∘ opNumEqualVerify)
    ∘ opTrue
  where
    polynomial :: F (S (s :> TInt) alt -> S (s :> TInt) alt)
    polynomial =
      fn
        ( begin
            ∘ ns #x
            ∘ (pick #x ∘ quartic)
            ∘ (pick #x ∘ cube ∘ int 10 ∘ opMul)
            ∘ (pick #x ∘ square ∘ int 35 ∘ opMul)
            ∘ (roll #x ∘ int 50 ∘ opMul)
            ∘ int 24
            ∘ (opAdd ∘ opAdd ∘ opAdd ∘ opAdd)
        )

    quartic :: Fn (s :> TInt) (s :> TInt)
    quartic = fn (square ∘ square)

    cube :: Fn (s :> TInt) (s :> TInt)
    cube = fn (opDup ∘ opDup ∘ opMul ∘ opMul)

    square :: Fn (s :> TInt) (s :> TInt)
    square = fn (opDup ∘ opMul)

progNestedCalls2 :: Fn s (s :> TBool)
progNestedCalls2 =
  begin
    ∘ (pushFe 3 ∘ pushFe 7 ∘ feMul ∘ pushFe 1 ∘ feAdd ∘ pushFe 12 ∘ feSub)
    ∘ (feCube ∘ pushFe 1000 ∘ equal)

progFactorial :: Fn s (s :> TBool)
progFactorial =
  begin
    ∘ (nat 0 ∘ fac ∘ nat 1 ∘ opNumEqual)
    ∘ (nat 1 ∘ fac ∘ nat 1 ∘ opNumEqual)
    ∘ (nat 6 ∘ fac ∘ nat 720 ∘ opNumEqual)
    ∘ (nat 10 ∘ fac ∘ nat 3_628_800 ∘ opNumEqual)
    ∘ (opBoolAnd ∘ opBoolAnd ∘ opBoolAnd)
  where
    fac :: Fn (s :> TNat) (s :> TNat)
    fac =
      fn
        ( begin
            ∘ (ns #n ∘ pick #n)
            ∘ ifZero
              (nat 1 ∘ del #n)
              (pick #n ∘ roll #n ∘ nat1SubUnsafe ∘ fac ∘ opMul)
        )

progSort :: Fn s (s :> TBool)
progSort =
  runEnv
    ( begin
        ∘ (nat 10 ∘ quot1 (toInt64 ∘ base ∘ add) ∘ generate)
        ∘ (opDup ∘ reverse ∘ MS.sort ∘ equal)
    )
  where
    toInt64 :: Fn (s :> TNat) (s :> TInt64)
    toInt64 = n2i ∘ fromInt

    base :: Fn s (s :> TInt64)
    base = int64 (2 ^ (48 :: Integer))

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 47) ==> do
    isTrue' $
      evaluateProg
        (bytes xs ∘ toVector ∘ MS.sort ∘ bytes (B.sort xs) ∘ toVector ∘ equal)
  where
    toVector :: Fn (s :> TBytes) (s :> TVector TInt8)
    toVector = cast

-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize progRuntimeFunctions
-- "37 opcodes, 37 bytes. Including function table: 55 opcodes, 196 bytes.\n"
progRuntimeFunctions :: FnA s Base (s :> TBool) Base
progRuntimeFunctions =
  runEnv
    ( begin
        ∘ (int 1 ∘ int 2 ∘ opAdd ∘ f ∘ apply2)
        ∘ (int 5 ∘ swap ∘ invoke1 ∘ int 2 ∘ opNumEqualVerify)
        ∘ (int 3 ∘ int 2 ∘ opAdd ∘ f ∘ apply2)
        ∘ (int 9 ∘ swap ∘ invoke1 ∘ int 4 ∘ opNumEqualVerify)
        ∘ (int 3 ∘ int 6 ∘ g ∘ apply3_2)
        ∘ (int 4 ∘ swap ∘ invoke1 ∘ opTrue ∘ opEqualVerify)
        ∘ opTrue
    )
  where
    f = quot2 opSub
    g = quot3 opWithin
