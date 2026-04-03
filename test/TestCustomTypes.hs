-- Copyright (c) 2025 albaDsl

module TestCustomTypes (testCustomTypes) where

import Alba.Dsl.V1.Bch2025
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)

data TAge

instance StackEntry TAge

instance StackNum TAge

instance StackNat TAge

testCustomTypes :: TestTree
testCustomTypes =
  testGroup
    "Custom types"
    [testCase "Custom int" $ isTrue (evaluateProg progAge)]

progAge :: Fn s (s > TBool)
progAge = begin # pushAge 32 # incAge # int 33 # cast # opNumEqual

pushAge :: Natural -> Fn s (s > TAge)
pushAge = nat'

incAge :: Fn (s > TAge) (s > TAge)
incAge = pushAge 1 # opAdd
