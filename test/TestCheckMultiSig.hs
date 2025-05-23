-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestCheckMultiSig (testCheckMultiSig) where

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
import Crypto.Secp256k1 (PubKey, SecKey)
import Crypto.Secp256k1.Internal.Context (withContext)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils qualified as TU

testCheckMultiSig :: TestTree
testCheckMultiSig =
  testGroup
    "CheckMultiSig"
    [ testCase "OpCheckMultiSig" $ test progCheckMultiSig2of3,
      testCase "OpCheckMultiSigVerify" $ test progCheckMultiSigVerify2of3
    ]

test ::
  ( forall s.
    Bytes ->
    Bytes ->
    Bytes ->
    FN
      (s > TNat > TSig > TSig > TNat)
      (s > TBool)
  ) ->
  Assertion
test prog = do
  kp1 <- fromMaybe (error "Failed to load keys.") <$> withContext genKey
  kp2 <- fromMaybe (error "Failed to load keys.") <$> withContext genKey
  kp3 <- fromMaybe (error "Failed to load keys.") <$> withContext genKey
  let (KeyPair sk1 pk1, KeyPair sk2 pk2, KeyPair _ pk3) = (kp1, kp2, kp3)
  [pk1', pk2', pk3'] <- mapM (exportPubKey False) ([pk1, pk2, pk3] :: [PubKey])
  let code = compile None (prog pk1' pk2' pk3')
      utxo = TU.utxoWithPubkey code
      txContext = TU.txContext utxo
  s1 <- signTx utxo sk1
  s2 <- signTx utxo sk2
  let state =
        (startState vmParamsStandard)
          { s =
              S.fromList
                [ i2SeUnsafe 0,
                  b2SeUnsafe s1,
                  b2SeUnsafe s2,
                  i2SeUnsafe 2
                ],
            alt = S.empty
          }
  let Right VmState {s, alt} = evaluateScript code txContext state
  (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty)

signTx :: TxOut -> SecKey -> IO Bytes
signTx utxo secKey = do
  withContext
    ( \ctx ->
        pure $
          marshal ctx (signAll ctx TU.tx utxo.scriptPubKey utxo 0 secKey)
    )

progCheckMultiSig2of3 ::
  Bytes ->
  Bytes ->
  Bytes ->
  FN (s > TNat > TSig > TSig > TNat) (s > TBool)
progCheckMultiSig2of3 key1 key2 key3 =
  begin
    # pubKeyBytes key1
    # pubKeyBytes key2
    # pubKeyBytes key3
    # nat 3
    # opCheckMultiSig @2 @3

progCheckMultiSigVerify2of3 ::
  Bytes ->
  Bytes ->
  Bytes ->
  FN (s > TNat > TSig > TSig > TNat) (s > TBool)
progCheckMultiSigVerify2of3 key1 key2 key3 =
  begin
    # pubKeyBytes key1
    # pubKeyBytes key2
    # pubKeyBytes key3
    # nat 3
    # opCheckMultiSigVerify @2 @3
    # opTrue
