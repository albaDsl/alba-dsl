-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Alba.Dsl.V1.Bch2025 (CodeL1, outputScript)
import Alba.Misc.Cmd (deployMsg, runInVm, showTx)
import Alba.Misc.Haskoin (Network, addrToText, chipnet, mainnet, textToAddr)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Tx.Bch2025 (OutPoint (..), TxOut (..), hash160)
import Alba.Vm.Bch2025 (Labels, LogDisplayOpts (..), defaultDisplayOpts)
import Alba.Vm.Common.Utils (pubKeyLabels)
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx, withContext)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Opts
  ( Command (..),
    ContractFunction (..),
    DeployOpts (..),
    Opts (..),
    RefreshOpts (..),
    SpendOpts (..),
    cmdOpts,
    execParser,
  )
import Params (fundingTxAmount, inheritKeys, refreshKeys, withdrawKeys)
import Spend (inheritTx, refreshTx, withdrawTx)
import System.Environment (withArgs)
import Test (contractTests)

data CmdContext = CmdContext
  { refresh :: KeyPair,
    withdraw :: KeyPair,
    inherit :: KeyPair,
    redeemScript :: CodeL1,
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
    Spend (Refresh (RefreshOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let outPoint = OutPoint (fromString txId) (fromIntegral utxoIndex)
          tx = refreshTx ctx outPoint c.utxo c.redeemScript c.refresh
          displayOpts = defaultDisplayOpts {labels = Just c.labels, showMetrics}
      if sim then runInVm displayOpts c.utxo tx else showTx tx
    Spend (Withdraw (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let recvAddr = case textToAddr c.net (pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          outPoint = OutPoint (fromString txId) (fromIntegral utxoIndex)
          tx =
            withdrawTx ctx outPoint c.utxo c.redeemScript c.withdraw recvAddr
          displayOpts = defaultDisplayOpts {labels = Just c.labels, showMetrics}
      if sim then runInVm displayOpts c.utxo tx else showTx tx
    Spend (Inherit (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let recvAddr = case textToAddr c.net (pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          outPoint = OutPoint (fromString txId) (fromIntegral utxoIndex)
          tx = inheritTx ctx outPoint c.utxo c.redeemScript c.inherit recvAddr
          displayOpts = defaultDisplayOpts {labels = Just c.labels, showMetrics}
      if sim then runInVm displayOpts c.utxo tx else showTx tx
    Test -> withArgs [] $ contractTests ctx

cmdContext :: Ctx -> Bool -> IO CmdContext
cmdContext ctx mainNetP = do
  let net = network mainNetP
  refresh <- fromMaybe err <$> refreshKeys net
  withdraw <- fromMaybe err <$> withdrawKeys net
  inherit <- fromMaybe err <$> inheritKeys net
  let (redeemScript, deployAddr) =
        instantiate ctx refresh.pubKey withdraw.pubKey inherit.pubKey
      labels =
        M.fromList
          [ (redeemScript, "redeem"),
            (hash160 redeemScript, "scriptHash")
          ]
          <> pubKeyLabels ctx refresh.pubKey "r"
          <> pubKeyLabels ctx withdraw.pubKey "w"
          <> pubKeyLabels ctx inherit.pubKey "i"
      utxo =
        TxOut
          { value = fundingTxAmount,
            scriptPubKey = outputScript deployAddr,
            tokenData = Nothing
          }
  pure
    CmdContext {deployAddr = fromJust $ addrToText net deployAddr, ..}
  where
    err = error "Failed to load keys."

    network :: Bool -> Network
    network True = mainnet
    network False = chipnet
