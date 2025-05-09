-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestCheckSig (testCheckSig) where

import Alba.Dsl.V1.Bch2025
import Alba.Misc.Haskoin (marshal)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Wallet (genKey)
import Alba.Tx.Bch2025 (TxOut (..))
import Alba.Vm.Bch2025
  ( VmState (..),
    b2SeUnsafe,
    evaluateScript,
    exportPubKey,
    i2SeUnsafe,
    startState,
    vmParamsStandard,
  )
import Crypto.Secp256k1 (SecKey)
import Crypto.Secp256k1.Internal.Context (withContext)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils qualified as TU

testCheckSig :: TestTree
testCheckSig =
  testGroup
    "CheckSig"
    [ testCase "OpCheckSig" $ test progCheckSig,
      testCase "OpCheckSigVerify" $ test progCheckSigVerify
    ]

test :: (forall s. Bytes -> FN (s > TSig) (s > TBool)) -> Assertion
test prog = do
  KeyPair secKey pubKey <-
    fromMaybe (error "Failed to load keys.") <$> withContext genKey
  pubKey' <- exportPubKey False pubKey
  let code = compile None (prog pubKey')
      utxo = TU.utxoWithPubkey code
      txContext = TU.txContext utxo
  sig <- signTx utxo secKey
  let state =
        (startState vmParamsStandard)
          { s = S.singleton (b2SeUnsafe sig),
            alt = S.empty
          }
  let Right VmState {s, alt} = evaluateScript code txContext False state
  (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty)

signTx :: TxOut -> SecKey -> IO Bytes
signTx utxo secKey = do
  withContext
    ( \ctx ->
        pure $
          marshal ctx (signAll ctx TU.tx utxo.scriptPubKey utxo 0 secKey)
    )

progCheckSig :: Bytes -> FN (s > TSig) (s > TBool)
progCheckSig pk = pubKeyBytes pk # opCheckSig

progCheckSigVerify :: Bytes -> FN (s > TSig) (s > TBool)
progCheckSigVerify pk = pubKeyBytes pk # opCheckSigVerify # opTrue
