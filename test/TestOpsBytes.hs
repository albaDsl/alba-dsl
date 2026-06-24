-- Copyright (c) 2025 albaDsl

module TestOpsBytes (testOpsBytes) where

import Alba.Dsl.V1.Bch2026
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
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils (evaluateProg, evaluateProgWithStack, getStack, isErr)

testOpsBytes :: TestTree
testOpsBytes =
  testGroup
    "Bytes ops"
    [ testProperty "Reverse" propReverse,
      testProperty "Size" propReverseSize,
      testProperty "Cat and split" propCatAndSplit,
      testCase "SePushSize" $ isErr (evaluateProg progPushSize) SePushSize
    ]

propReverse :: Bytes -> Bool
propReverse x =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      s = getStack $ evaluateProgWithStack prog stack
   in s == S.singleton (b2SeUnsafe x)
  where
    prog :: Fn (s :> TBytes) (s :> TBytes)
    prog = opReverseBytes ∘ opReverseBytes

propReverseSize :: Bytes -> Bool
propReverseSize x =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      s = getStack $ evaluateProgWithStack prog stack
   in s == S.singleton (i2SeUnsafe $ fromIntegral (B.length x))
  where
    prog :: Fn (s :> TBytes) (s :> TNat)
    prog =
      begin
        ∘ opSize -- b s
        ∘ opSwap -- s b
        ∘ opReverseBytes -- s br
        ∘ opSize -- s br s
        ∘ opSwap -- s s br
        ∘ opDrop -- s s
        ∘ opDup -- s s s
        ∘ opNumEqualVerify -- s

propCatAndSplit :: BytesHalf -> Bool
propCatAndSplit (BytesHalf x) =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      s = getStack $ evaluateProgWithStack prog stack
   in s == S.singleton (boolToStackElement True)
  where
    prog :: Fn (s :> TBytes) (s :> TBool)
    prog =
      begin -- b
        ∘ opSize -- b s
        ∘ opSwap -- s b
        ∘ opDup -- s b b
        ∘ opCat -- s bb
        ∘ opSwap -- bb s
        ∘ opSplit -- b b
        ∘ opEqual -- t

progPushSize :: Fn s (s :> TBytes)
progPushSize =
  begin
    ∘ bytes (B.replicate (maxBytes `div` 2) 0)
    ∘ opDup
    ∘ bytes (B.singleton 0)
    ∘ opCat
    ∘ opCat
  where
    maxBytes = VP.vmParamsStandard.maxScriptElementSize
