-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestCustomTypes (testCustomTypes) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Bch2025 (CodeL2, OpcodeL2 (OP_1, OP_ADD, OP_DATA), i2SeUnsafe)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Data.Sequence qualified as S
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProg)

data TAge

instance StackEntry TAge

instance StackNum TAge

instance StackNat TAge

testCustomTypes :: TestTree
testCustomTypes =
  testGroup
    "Custom types"
    [ testCase "Custom int" $
        let Right (s, alt) = evaluateProg progAge
            c = compileL2 None progAge
         in (s, alt, c)
              @?= ( S.singleton (i2SeUnsafe 33),
                    S.empty,
                    progAgeCode
                  )
    ]

progAge :: FN s (s > TAge)
progAge = begin # pushAge 32 # incAge

pushAge :: Natural -> FN s (s > TAge)
pushAge = nat'

incAge :: FN (s > TAge) (s > TAge)
incAge = pushAge 1 # opAdd

progAgeCode :: CodeL2
progAgeCode = S.fromList [OP_DATA L1.OP_DATA_01 [32], OP_1, OP_ADD]
