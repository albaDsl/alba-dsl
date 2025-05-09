-- Copyright (c) 2025 albaDsl

-- {-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

-- {-@ LIQUID "--exact-data-cons" @-}

module Test (contractTests) where

import Alba.Dsl.V1.Bch2025 (outputScript)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Misc.Wallet (genKey)
import Alba.Tx.Bch2025 (OutPoint (..), TxOut (..))
import Alba.Vm.Bch2025 (mkTxContext, verifyScript)
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx, PubKey, SecKey)
import Data.Either (isRight)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Spend (senderWithdrawTx)
import Test.QuickCheck
  ( Args (..),
    NonNegative (..),
    Property,
    Result (Success),
    quickCheckWithResult,
    stdArgs,
    tabulate,
    (===),
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

{-@ ignore propSenderWithdraw @-}

contractTests :: Ctx -> IO ()
contractTests ctx =
  defaultMain $
    testGroup
      "TransferWithTimeout"
      [ testCase "senderWithdraw with different timeouts" $ do
          res <- quickCheckTests ctx
          res @?= True
      ]

quickCheckTests :: Ctx -> IO Bool
quickCheckTests ctx = do
  (KeyPair _ recipientPub, KeyPair senderSec senderPub) <- keyPairs ctx
  res <-
    quickCheckWithResult
      ( stdArgs
          { maxSuccess = 1000,
            maxSize = fromIntegral (maxBound :: Word32) :: Int
          }
      )
      (propSenderWithdraw ctx senderPub recipientPub senderSec)
  case res of
    Success {} -> pure True
    _ -> pure False

propSenderWithdraw ::
  Ctx -> PubKey -> PubKey -> SecKey -> NonNegative Int -> Property
propSenderWithdraw ctx senderPub recipientPub senderSec timeout =
  let NonNegative timeout' = timeout
      cutoff = 500_000_000
   in tabulate
        "timeouts"
        (if timeout' < cutoff then ["Block height"] else ["UNIX timestamp"])
        (test $ fromIntegral timeout')
  where
    test :: Natural -> Property
    test timeout'' =
      let (redeemScript, deployAddr) =
            instantiate ctx senderPub recipientPub timeout''
          utxo =
            TxOut
              { value = 10_000,
                scriptPubKey = outputScript deployAddr,
                tokenData = Nothing
              }
          tx =
            senderWithdrawTx
              ctx
              (OutPoint mockTxId 0)
              utxo
              redeemScript
              senderSec
              timeout''
              mockAddr
          context = fromJust $ mkTxContext tx 0 [utxo]
       in isRight (verifyScript utxo.scriptPubKey context vmParamsStandard)
            === True

keyPairs :: Ctx -> IO (KeyPair, KeyPair)
keyPairs ctx = do
  sender <- fromJust <$> genKey ctx
  recipient <- fromJust <$> genKey ctx
  pure (sender, recipient)
