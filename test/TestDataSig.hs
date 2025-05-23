-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestDataSig (testDataSig) where

import Alba.Dsl.V1.Bch2025
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Wallet (genKey)
import Alba.Tx.Bch2025.Hash (sha256)
import Alba.Vm.Common (exportPubKey, i2SeUnsafe, signDer)
import Crypto.Secp256k1 (withContext)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Data.Text.Encoding qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils (evaluateProg)

testDataSig :: TestTree
testDataSig =
  testGroup
    "DataSig"
    [ testCase "opCheckDataSig" $ test progCheckDataSig,
      testCase "opCheckDataSigVerify" $ test progCheckDataSigVerify
    ]

test :: (forall s. Bytes -> Bytes -> Bytes -> FN s (s > TBool)) -> Assertion
test prog = do
  KeyPair secKey pubKey <-
    fromMaybe (error "Failed to load keys.") <$> withContext genKey
  let msg = T.encodeUtf8 "hello world"
  let hash = sha256 msg
  sigData <- signDer hash secKey
  pubKey' <- exportPubKey False pubKey
  let Right (s, alt) = evaluateProg (prog sigData msg pubKey')
  (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty)

progCheckDataSig :: Bytes -> Bytes -> Bytes -> FN s (s > TBool)
progCheckDataSig sig msg pk =
  begin
    # sigBytes sig
    # bytes msg
    # pubKeyBytes pk
    # opCheckDataSig

progCheckDataSigVerify :: Bytes -> Bytes -> Bytes -> FN s (s > TBool)
progCheckDataSigVerify sig msg pk =
  begin
    # sigBytes sig
    # bytes msg
    # pubKeyBytes pk
    # opCheckDataSigVerify
    # opTrue
