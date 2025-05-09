-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestOpsHash (testOpsHash) where

import Alba.Dsl.V1.Bch2025
import Alba.Tx.Bch2025.Hash (hash160, hash256, ripemd160, sha1, sha256)
import Alba.Vm.Bch2025 (b2SeUnsafe)
import Data.Sequence qualified as S
import QuickCheckSupport (BytesHalf (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils (evaluateProgWithStack)

testOpsHash :: TestTree
testOpsHash =
  testGroup
    "Crypto Ops"
    [ testProperty "opRipemd160" (propHash opRipemd160 ripemd160),
      testProperty "opSha1" (propHash opSha1 sha1),
      testProperty "opSha256" (propHash opSha256 sha256),
      testProperty "opHash160" (propHash opHash160 hash160),
      testProperty "opHash256" (propHash opHash256 hash256)
    ]

propHash ::
  (StackBytes a) =>
  FN (s > a) (s > a) ->
  (Bytes -> Bytes) ->
  BytesHalf ->
  Bool
propHash op f (BytesHalf x) =
  let stack = (S.singleton $ b2SeUnsafe x, S.empty)
      Right (s, _) = evaluateProgWithStack op stack
   in s == S.singleton (b2SeUnsafe (f x))
