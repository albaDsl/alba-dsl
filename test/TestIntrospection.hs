-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestIntrospection (testIntrospection) where

import Alba.Dsl.V1.Bch2025
import Alba.Misc.MockVals (mockTxId)
import Alba.Tx.Bch2025 (Hash256 (..), TxId (..))
import Alba.Vm.Common (b2SeUnsafe, i2SeUnsafe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateScript, txContext, utxoWithPubkey)

testIntrospection :: TestTree
testIntrospection =
  testGroup
    "Introspection"
    [ testCase "OP_ACTIVEBYTECODE" $ do
        let code = compile None (opActiveBytecode # opEqual)
            ctx = txContext (utxoWithPubkey code)
            stacks = (S.singleton (b2SeUnsafe code), S.empty)
            Right (s, alt) = evaluateScript code stacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty),
      testCase "OP_TXVERSION" $ do
        let code = compile None opTxVersion
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 2), S.empty),
      testCase "OP_TXINPUTCOUNT" $ do
        let code = compile None opTxInputCount
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty),
      testCase "OP_TXOUTPUTCOUNT" $ do
        let code = compile None opTxOutputCount
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty),
      testCase "OP_OUTPOINTTXHASH" $ do
        let code = compile None (op0 # opOutPointTxHash)
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (b2SeUnsafe mockTxId.id.hash), S.empty),
      testCase "OP_TXOUTPOINTINDEX" $ do
        let code = compile None (op0 # opOutPointIndex)
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 0), S.empty),
      testCase "OP_INPUTBYTECODE" $ do
        let code = compile None (op0 # opInputBytecode)
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (b2SeUnsafe ""), S.empty),
      testCase "OP_INPUTSEQUENCENUMBER" $ do
        let code = compile None (op0 # opInputSequenceNumber)
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 0), S.empty),
      testCase "OP_CHECKLOCKTIMEVERIFY" $ do
        let code = compile None (op0 # opCheckLockTimeVerify # opDrop # opTrue)
            ctx = txContext (utxoWithPubkey code)
            Right (s, alt) = evaluateScript code emptyStacks ctx
        (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty)
    ]
  where
    emptyStacks = (S.empty, S.empty)
