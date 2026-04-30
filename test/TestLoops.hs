-- Copyright (c) 2025 albaDsl

module TestLoops (testLoops) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( factorial,
    iterate,
    nat1SubUnsafe,
    pow,
  )
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (iterate)

testLoops :: TestTree
testLoops =
  testGroup
    "Loops"
    [ testCase "Loops - factorial 1" $ isTrue (evaluateProg progFactorial1),
      testCase "Loops - factorial 2" $ isTrue (evaluateProg progFactorial2),
      testCase "Loops - factorial 3" $ isTrue (evaluateProg progFactorial3),
      testProperty "Loops — pow" propPow
    ]

progFactorial1 :: Fn s (s :> TBool)
progFactorial1 = progFacTest fac
  where
    fac :: Natural -> Fn s (s :> TNat)
    fac n = nat n ∘ factorial

progFacTest :: (forall s'. Natural -> Fn s' (s' :> TNat)) -> Fn s (s :> TBool)
progFacTest fac =
  begin
    ∘ (fac 0 ∘ nat 1 ∘ opNumEqual)
    ∘ (fac 1 ∘ nat 1 ∘ opNumEqual)
    ∘ (fac 6 ∘ nat 720 ∘ opNumEqual)
    ∘ (fac 10 ∘ nat 3_628_800 ∘ opNumEqual)
    ∘ (opBoolAnd ∘ opBoolAnd ∘ opBoolAnd)

progFactorial2 :: Fn s (s :> TBool)
progFactorial2 = progFacTest fac
  where
    fac :: Natural -> Fn s (s :> TNat)
    fac n = nat 1 ∘ nat n ∘ iterate n f ∘ opDrop

    f :: Fn (s :> TNat :> TNat) (s :> TNat :> TNat)
    f =
      begin
        ∘ (ns2 #product #n ∘ pick #n ∘ roll #product ∘ opMul)
        ∘ (roll #n ∘ nat1SubUnsafe)

progFactorial3 :: Fn s (s :> TBool)
progFactorial3 = progFacTest fac
  where
    fac :: Natural -> Fn s (s :> TNat)
    fac n = nat 1 ∘ iterate n f

    f :: FnA (s :> TNat) (alt :> TNat) (s :> TNat) (alt :> TNat)
    f =
      begin
        ∘ (ns #product ∘ opFromAltStack ∘ opDup ∘ opToAltStack)
        ∘ (roll #product ∘ opMul)

propPow :: Int -> Word8 -> Bool
propPow b n =
  let expected = (fromIntegral b :: Integer) ^ (fromIntegral n :: Integer)
      prog =
        begin
          ∘ (int (fromIntegral b) ∘ nat (fromIntegral n) ∘ pow)
          ∘ (int expected ∘ opNumEqual)
   in isTrue' $ evaluateProg prog
