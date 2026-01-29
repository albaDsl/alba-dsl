-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test (contractTests) where

import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxOut (..))
import Alba.Vm.Bch2026
  ( LogDisplayOpts (..),
    defaultDisplayOpts,
    dumpVerifyScriptResult,
    mkTxContext,
    verifyScript,
  )
import Alba.Vm.Bch2026.VmParams (vmParamsStandard)
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx)
import Data.Either (isRight)
import Data.Maybe (fromJust)
import Spend (withdrawTx)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

contractTests :: Ctx -> IO ()
contractTests ctx =
  defaultMain $
    testGroup
      "permutationChallenge"
      [ testCase "Can withdraw" $ do
          let contractOutpoint = OutPoint mockTxId 0
              tx =
                withdrawTx
                  ctx
                  contractOutpoint
                  contractUtxo.value
                  mockTxId
                  mockTxId
                  mockAddr
              coins =
                Dc.deployTx.outputs <> Vc.deployTx.outputs <> [contractUtxo]
              context = fromJust $ mkTxContext tx inputId coins
              res = verifyScript contractUtxo.scriptPubKey context params
          -- dumpVerifyScriptResult
          --   (defaultDisplayOpts {showMetrics = True})
          --   res
          isRight res == True @?= True
      ]
  where
    params = vmParamsStandard
    inputId = fromIntegral (Dc.numUtxos + Vc.numUtxos)

    -- The UTXO holding the funds protected by the permutation challenge.
    contractUtxo :: TxOut
    contractUtxo =
      TxOut {value = 10_000, scriptPubKey = instantiate, tokenData = Nothing}
