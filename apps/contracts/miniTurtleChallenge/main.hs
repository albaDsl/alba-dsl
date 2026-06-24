-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Alba.Dsl.V1.Bch2026 (CodeL1, outputScript)
import Alba.Misc.Cmd (deployMsg, runInVm, showTx)
import Alba.Misc.Haskoin (Network, addrToText, chipnet, mainnet, textToAddr)
import Alba.Tx.Bch2025 (OutPoint (..), TxOut (..), hash160)
import Alba.Vm.Bch2025 (Labels, LogDisplayOpts (..), defaultDisplayOpts)
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx, withContext)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Opts
  ( Command (..),
    ContractFunction (..),
    DeployOpts (..),
    Opts (..),
    SpendOpts (..),
    cmdOpts,
    execParser,
  )
import Params (fundingTxAmount)
import Spend (withdrawTx)
import System.Environment (withArgs)
import Test (contractTests)

data CmdContext = CmdContext
  { redeemScript :: CodeL1,
    deployAddr :: Text,
    utxo :: TxOut,
    labels :: Labels,
    net :: Network
  }

main :: IO ()
main = do
  opts <- execParser cmdOpts
  withContext (main' opts)

main' :: Opts -> Ctx -> IO ()
main' opts ctx = do
  case opts.command of
    Deploy (DeployOpts {..}) -> do
      c <- cmdContext ctx mainNet
      deployMsg fundingTxAmount c.deployAddr (Just (B.length c.redeemScript))
    Spend (Withdraw (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let recvAddr = case textToAddr c.net (pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          outPoint = OutPoint (fromString txId) (fromIntegral utxoIndex)
          tx = withdrawTx ctx outPoint c.utxo c.redeemScript recvAddr
          displayOpts = defaultDisplayOpts {labels = Just c.labels, showMetrics}
      if sim then runInVm displayOpts c.utxo tx else showTx tx
    Test -> withArgs [] $ contractTests ctx

cmdContext :: Ctx -> Bool -> IO CmdContext
cmdContext ctx mainNetP = do
  let net = network mainNetP
      (redeemScript, deployAddr) = instantiate ctx
      labels =
        M.fromList
          [ (redeemScript, "redeem"),
            (hash160 redeemScript, "scriptHash")
          ]
      utxo =
        TxOut
          { value = fundingTxAmount,
            scriptPubKey = outputScript deployAddr,
            tokenData = Nothing
          }
  pure
    CmdContext {deployAddr = fromJust $ addrToText net deployAddr, ..}
  where
    network :: Bool -> Network
    network True = mainnet
    network False = chipnet
