-- Copyright (c) 2026 albaDsl

module TestLibMisc (testLibMisc) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Misc (addToUnsigned)
import Alba.Vm.Common.VmInteger (integerToBytesUnsigned)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), testProperty)
import TestUtils2026 (evaluateProg, isTrue')
import Prelude

testLibMisc :: TestTree
testLibMisc =
  testGroup
    "Misc"
    [ testProperty "addToUnsigned" propAddToUnsigned
    ]

propAddToUnsigned :: NonNegative Int -> NonNegative Int -> Bool
propAddToUnsigned (NonNegative n) (NonNegative n') =
  let expected = fromIntegral (n + n')
      prog =
        begin
          ∘ nat (fromIntegral n)
          ∘ bytes (integerToBytesUnsigned (fromIntegral n'))
          ∘ addToUnsigned
          ∘ bytes (integerToBytesUnsigned expected)
          ∘ opEqual
   in isTrue' $ evaluateProg prog
