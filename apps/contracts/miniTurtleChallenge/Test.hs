-- Copyright (c) 2025 albaDsl

module Test (contractTests) where

import Alba.Dsl.V1.Bch2025 (outputScript)
import Alba.Misc.Haskoin (Address)
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Tx.Bch2025 (OutPoint (..), TxOut (..))
import Alba.Vm.Bch2025 (mkTxContext, verifyScript)
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx)
import Data.Either (isRight)
import Data.Maybe (fromJust)
import Spend (withdrawTx)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (seq)

contractTests :: Ctx -> IO ()
contractTests ctx =
  defaultMain $
    testGroup
      "miniTurtleChallenge"
      [ testCase "Can withdraw" $ do
          let (redeemScript, deployAddr) = instantiate ctx
              utxo = createUtxo deployAddr
              outPoint = OutPoint mockTxId 0
              tx = withdrawTx ctx outPoint utxo redeemScript mockAddr
              context = fromJust $ mkTxContext tx 0 [utxo]
          isRight (verifyScript utxo.scriptPubKey context params) == True
            @?= True
      ]
  where
    params = vmParamsStandard

createUtxo :: Address -> TxOut
createUtxo deployAddr =
  TxOut
    { value = 10_000,
      scriptPubKey = outputScript deployAddr,
      tokenData = Nothing
    }
