-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestOpsBytes (testOpsBytes) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Bch2025.VmParams qualified as VP
import Alba.Vm.Common
  ( ScriptError (SePushSize),
    b2SeUnsafe,
    boolToStackElement,
    i2SeUnsafe,
  )
import Alba.Vm.Common.VmParams (VmParams (..))
import Data.ByteString qualified as B
import Data.Sequence qualified as S
import QuickCheckSupport (BytesHalf (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestUtils (evaluateProg, evaluateProgWithStack)

testOpsBytes :: TestTree
testOpsBytes =
  testGroup
    "Bytes ops"
    [ testProperty "Reverse" propReverse,
      testProperty "Size" propReverseSize,
      testProperty "Cat and split" propCatAndSplit,
      testCase "SePushSize" $ evaluateProg progPushSize @?= Left SePushSize
    ]

propReverse :: Bytes -> Bool
propReverse x =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      Right (s, _) = evaluateProgWithStack prog stack
   in s == S.singleton (b2SeUnsafe x)
  where
    prog :: FN (s > TBytes) (s > TBytes)
    prog = opReverseBytes # opReverseBytes

propReverseSize :: Bytes -> Bool
propReverseSize x =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      Right (s, _) = evaluateProgWithStack prog stack
   in s == S.singleton (i2SeUnsafe $ fromIntegral (B.length x))
  where
    prog :: FN (s > TBytes) (s > TNat)
    prog =
      begin
        # opSize -- b s
        # opSwap -- s b
        # opReverseBytes -- s br
        # opSize -- s br s
        # opSwap -- s s br
        # opDrop -- s s
        # opDup -- s s s
        # opEqualVerify -- s

propCatAndSplit :: BytesHalf -> Bool
propCatAndSplit (BytesHalf x) =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      Right (s, _) = evaluateProgWithStack prog stack
   in s == S.singleton (boolToStackElement True)
  where
    prog :: FN (s > TBytes) (s > TBool)
    prog =
      begin -- b
        # opSize -- b s
        # opSwap -- s b
        # opDup -- s b b
        # opCat -- s bb
        # opSwap -- bb s
        # opSplit -- b b
        # opEqual -- t

progPushSize :: FN s (s > TBytes)
progPushSize =
  begin
    # bytes (B.replicate (maxBytes `div` 2) 0)
    # opDup
    # bytes (B.singleton 0)
    # opCat
    # opCat
  where
    maxBytes = VP.vmParamsStandard.maxScriptElementSize
