-- Copyright (c) 2026 albaDsl

module TestRuntimeLib (testRuntimeLib) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Alba.Vm.Common (b2SeUnsafe)
import Data.ByteString qualified as B
import Data.Sequence qualified as S
import QuickCheckSupport ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import TestUtils2026 (evaluateProgWithStack, isTrue')
import Prelude hiding (drop, fst, snd, sum)

testRuntimeLib :: TestTree
testRuntimeLib =
  testGroup "Runtime Lib" [testProperty "toPushOp" propToPushOp]

propToPushOp :: Bytes -> Property
propToPushOp b =
  (B.length b <= 9997) ==>
    isTrue' (evaluateProgWithStack prog (S.singleton $ b2SeUnsafe b, S.empty))
  where
    prog :: Fn (s > TBytes) (s > TBool)
    prog =
      begin
        # (opDup # toPushOp # opDefineNamed "f")
        # opInvokeNamed "f" f
        # opEqual

    f :: Fn s (s > TBytes)
    f = undefined
