-- Copyright (c) 2025 albaDsl

-- {-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

-- {-@ LIQUID "--exact-data-cons" @-}

module Test (contractTests) where

import Alba.Dsl.V1.Bch2025 (outputScript, setScriptSig, signAll, timeSequence)
import Alba.Misc.Haskoin (Address, scriptAddress)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Misc.Wallet (genKey)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025 (mkTxContext, verifyScript)
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import Alba.Vm.Common (Bytes)
import ContractApi (instantiate, refresh)
import Crypto.Secp256k1 (Ctx)
import Data.ByteString qualified as B
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Params (fee, refreshDelay)
import Spend (inheritTx, refreshTx, withdrawTx)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (seq)

contractTests :: Ctx -> IO ()
contractTests ctx =
  defaultMain $
    testGroup
      "LastWill"
      [ testCase "Can refresh" $ do
          (kp@(KeyPair _ rPub), KeyPair _ wPub, KeyPair _ iPub) <- keyPairs ctx
          let (redeemScript, deployAddr) = instantiate ctx rPub wPub iPub
              utxo = createUtxo deployAddr
              tx = refreshTx ctx (OutPoint mockTxId 0) utxo redeemScript kp
              context = fromJust $ mkTxContext tx 0 [utxo]
          isRight (verifyScript utxo.scriptPubKey context params) == True
            @?= True,
        testCase "Can withdraw" $ do
          (KeyPair _ rPub, kp@(KeyPair _ wPub), KeyPair _ iPub) <- keyPairs ctx
          let (redeemScript, deployAddr) = instantiate ctx rPub wPub iPub
              utxo = createUtxo deployAddr
              outPoint = OutPoint mockTxId 0
              tx = withdrawTx ctx outPoint utxo redeemScript kp mockAddr
              context = fromJust $ mkTxContext tx 0 [utxo]
          isRight (verifyScript utxo.scriptPubKey context params) == True
            @?= True,
        testCase "Can inherit" $ do
          (KeyPair _ rPub, KeyPair _ wPub, kp@(KeyPair _ iPub)) <- keyPairs ctx
          let (redeemScript, deployAddr) = instantiate ctx rPub wPub iPub
              utxo = createUtxo deployAddr
              outPoint = OutPoint mockTxId 0
              tx = inheritTx ctx outPoint utxo redeemScript kp mockAddr
              context = fromJust $ mkTxContext tx 0 [utxo]
          isRight (verifyScript utxo.scriptPubKey context params) == True
            @?= True,
        testCase "Can't use refresh function to withdraw — 1" $ do
          (kp@(KeyPair _ rPub), KeyPair _ wPub, KeyPair _ iPub) <- keyPairs ctx
          let (redeemScript, deployAddr) = instantiate ctx rPub wPub iPub
              utxo = createUtxo deployAddr
              tx = stealTx1 ctx utxo redeemScript kp
              context = fromJust $ mkTxContext tx 0 [utxo]
          isLeft (verifyScript utxo.scriptPubKey context params) == True
            @?= True,
        testCase "Can't use refresh function to withdraw — 2" $ do
          (kp@(KeyPair _ rPub), KeyPair _ wPub, KeyPair _ iPub) <- keyPairs ctx
          let (redeemScript, deployAddr) = instantiate ctx rPub wPub iPub
              utxo = createUtxo deployAddr
              tx = stealTx2 ctx utxo redeemScript kp
              context = fromJust $ mkTxContext tx 0 [utxo]
          isLeft (verifyScript utxo.scriptPubKey context params) == True
            @?= True
      ]
  where
    params = vmParamsStandard

keyPairs :: Ctx -> IO (KeyPair, KeyPair, KeyPair)
keyPairs ctx = do
  refreshKey <- fromJust <$> genKey ctx
  withdrawKey <- fromJust <$> genKey ctx
  inheritKey <- fromJust <$> genKey ctx
  pure (refreshKey, withdrawKey, inheritKey)

createUtxo :: Address -> TxOut
createUtxo deployAddr =
  TxOut
    { value = 10_000,
      scriptPubKey = outputScript deployAddr,
      tokenData = Nothing
    }

-- Invokes the refresh function but outputs the funds to a withdrawal address
-- instead of back to the contract itself.
stealTx1 :: Ctx -> TxOut -> Bytes -> KeyPair -> Tx
stealTx1 ctx utxo redeemScript kp =
  let seq = fromIntegral $ timeSequence refreshDelay
      tx = txTemplate seq (utxo.value - fee) (outputScript mockAddr)
      sig = signAll ctx tx redeemScript utxo 0 kp.secKey
   in setScriptSig 0 (refresh ctx redeemScript kp.pubKey sig) tx
  where
    txTemplate :: Natural -> Word64 -> B.ByteString -> Tx
    txTemplate seq outAmount scriptPubKey =
      Tx
        { version = 2,
          inputs =
            [ TxIn
                { prevout = OutPoint {txId = mockTxId, index = 0},
                  scriptSig = [],
                  sequence = fromIntegral seq
                }
            ],
          outputs =
            [ TxOut
                { value = outAmount,
                  scriptPubKey = scriptPubKey,
                  tokenData = Nothing
                }
            ],
          lockTime = 0
        }

-- Invokes the refresh function, outputs a small amount back to the contract and
-- a large to a withdrawal address.
stealTx2 :: Ctx -> TxOut -> Bytes -> KeyPair -> Tx
stealTx2 ctx utxo redeemScript kp =
  let seq = fromIntegral $ timeSequence refreshDelay
      tx =
        txTemplate
          seq
          utxo.value
          (outputScript mockAddr)
          (outputScript (scriptAddress redeemScript))
      sig = signAll ctx tx redeemScript utxo 0 kp.secKey
   in setScriptSig 0 (refresh ctx redeemScript kp.pubKey sig) tx
  where
    txTemplate ::
      Natural ->
      Word64 ->
      B.ByteString ->
      B.ByteString ->
      Tx
    txTemplate seq utxoValue scriptPubKey scriptPubKey' =
      Tx
        { version = 2,
          inputs =
            [ TxIn
                { prevout = OutPoint {txId = mockTxId, index = 0},
                  scriptSig = [],
                  sequence = fromIntegral seq
                }
            ],
          outputs =
            [ TxOut
                { value = fee,
                  scriptPubKey = scriptPubKey,
                  tokenData = Nothing
                },
              TxOut
                { value = utxoValue - 2 * fee,
                  scriptPubKey = scriptPubKey',
                  tokenData = Nothing
                }
            ],
          lockTime = 0
        }
