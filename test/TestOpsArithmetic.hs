-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-orphans #-}

module TestOpsArithmetic (testOpsArithmetic) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Bch2025
  ( ScriptError (SeDivideByZero, SeModByZero),
    b2SeUnsafe,
    boolToStackElement,
    i2SeUnsafe,
  )
import Data.Maybe (fromJust, isNothing)
import Data.Sequence qualified as S
import QuickCheckSupport (genByteStringOfSize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary,
    Gen,
    arbitrary,
    choose,
    oneof,
    resize,
    sized,
    testProperty,
  )
import TestUtils (evaluateProgWithStack)
import Prelude hiding (exp)

testOpsArithmetic :: TestTree
testOpsArithmetic =
  testGroup
    "Arithmetic"
    [ testProperty "Integer arithmetic" propInt,
      testProperty "Bool and integer arithmetic" propBool,
      testProperty "Bitwise arithmetic" propBits
    ]

propInt :: IntExp -> Bool
propInt e =
  let res = evaluateProgWithStack (intExp e) (S.empty, S.empty)
   in case res of
        Right (s, _alt) ->
          s == S.singleton (i2SeUnsafe . fromJust $ evalIntExp e)
        Left SeDivideByZero -> isNothing $ evalIntExp e
        Left SeModByZero -> isNothing $ evalIntExp e
        Left err -> error ("Unexpected evavaluation failure: " <> show err)

propBool :: BoolExp -> Bool
propBool e =
  let res = evaluateProgWithStack (boolExp e) (S.empty, S.empty)
   in case res of
        Right (s, _alt) ->
          s == S.singleton (boolToStackElement $ fromJust $ evalBoolExp e)
        Left SeDivideByZero -> isNothing $ evalBoolExp e
        Left SeModByZero -> isNothing $ evalBoolExp e
        Left err -> error ("Unexpected evavaluation failure: " <> show err)

propBits :: BitExp -> Bool
propBits e =
  let res = evaluateProgWithStack (bitExp e) (S.empty, S.empty)
   in case res of
        Right (s, _alt) -> s == S.singleton (b2SeUnsafe $ evalBitExp e)
        Left err -> error ("Unexpected evavaluation failure: " <> show err)

instance Arbitrary IntExp where
  arbitrary = resize 7 $ sized go
    where
      go 0 = do
        let maxVal = 2 ^ (128 :: Int) :: Integer
        x <- choose (-maxVal, maxVal) :: Gen Integer
        pure $ Int x
      go n =
        oneof
          [ Int <$> arbitrary,
            (:+) <$> go' <*> go',
            (:-) <$> go' <*> go',
            (:*) <$> go' <*> go',
            (:/) <$> go' <*> go',
            (:%) <$> go' <*> go',
            Abs <$> go',
            Neg <$> go',
            Pred <$> go',
            Succ <$> go',
            Min <$> go' <*> go',
            Max <$> go' <*> go'
          ]
        where
          go' = go (n - 1)

instance Arbitrary BoolExp where
  arbitrary = resize 7 $ sized go
    where
      go 0 = Bool <$> arbitrary
      go n =
        oneof
          [ Bool <$> arbitrary,
            (:&&) <$> go' <*> go',
            (:||) <$> go' <*> go',
            Not <$> go',
            (:==) <$> goArith <*> goArith,
            (:/=) <$> goArith <*> goArith,
            (:<) <$> goArith <*> goArith,
            (:>) <$> goArith <*> goArith,
            (:<=) <$> goArith <*> goArith,
            (:>=) <$> goArith <*> goArith,
            Within <$> goArith <*> goArith <*> goArith,
            ZeroNotEqual <$> goArith
          ]
        where
          go' = go (n - 1)
          goArith = resize (n - 1) arbitrary

instance Arbitrary BitExp where
  arbitrary = do
    maxBytes <- choose (0, 32) :: Gen Int
    resize 7 $ sized (go maxBytes)
    where
      go maxBytes 0 = Bytes <$> genByteStringOfSize maxBytes
      go maxBytes n =
        oneof
          [ Bytes <$> genByteStringOfSize maxBytes,
            (:&) <$> go' <*> go',
            (:|) <$> go' <*> go',
            Xor <$> go' <*> go'
          ]
        where
          go' = go maxBytes (n - 1)
