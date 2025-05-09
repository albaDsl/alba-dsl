-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Alba.Dsl.V1.Bch2025 (CodeL1, outputScript)
import Alba.Misc.Cmd (deployMsg, runInVm, showTx)
import Alba.Misc.Haskoin (Network, addrToText, chipnet, mainnet, textToAddr)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Tx.Bch2025 (OutPoint (..), TxId, TxOut (..), hash160)
import Alba.Vm.Bch2025
  ( Labels,
    LogDisplayOpts (..),
    defaultDisplayOpts,
    integerToBytes,
  )
import Alba.Vm.Common.Utils (pubKeyLabels)
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx, withContext)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Numeric.Natural (Natural)
import Opts
  ( Command (..),
    ContractFunction (..),
    DeployOpts (..),
    Opts (..),
    SpendOpts (..),
    cmdOpts,
    execParser,
  )
import Params
  ( fundingTxAmount,
    recipientKeys,
    senderKeys,
    timeoutStr,
  )
import Spend (recipientWithdrawTx, senderWithdrawTx)
import System.Environment (withArgs)
import Test (contractTests)

data CmdContext = CmdContext
  { recipient :: KeyPair,
    sender :: KeyPair,
    tmo :: Natural,
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
    Spend (RecipientWithdraw (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let txId' = fromString txId :: TxId
          recvAddr = case textToAddr c.net (T.pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          outpoint = OutPoint txId' (fromIntegral utxoIndex)
          secKey = c.recipient.secKey
          script = c.redeemScript
          tx = recipientWithdrawTx ctx outpoint c.utxo script secKey recvAddr
          displayOpts = defaultDisplayOpts {labels = Just c.labels, showMetrics}
      if sim then runInVm displayOpts c.utxo tx else showTx tx
    Spend (SenderWithdraw (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let txId' = fromString txId :: TxId
          recvAddr = case textToAddr c.net (T.pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          outpoint = OutPoint txId' (fromIntegral utxoIndex)
          secKey = c.recipient.secKey
          script = c.redeemScript
          tx = senderWithdrawTx ctx outpoint c.utxo script secKey c.tmo recvAddr
          displayOpts = defaultDisplayOpts {labels = Just c.labels, showMetrics}
      if sim then runInVm displayOpts c.utxo tx else showTx tx
    Test -> withArgs [] $ contractTests ctx

cmdContext :: Ctx -> Bool -> IO CmdContext
cmdContext ctx mainNetP = do
  let net = network mainNetP
  sender <- fromMaybe err <$> senderKeys net
  recipient <- fromMaybe err <$> recipientKeys net
  tmo <-
    floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
      <$> iso8601ParseM timeoutStr
  let (redeemScript, deployAddr) =
        instantiate ctx sender.pubKey recipient.pubKey tmo
      labels =
        M.fromList
          [ (redeemScript, "redeem"),
            (hash160 redeemScript, "scriptHash"),
            (integerToBytes (fromIntegral tmo), "tmo")
          ]
          <> pubKeyLabels ctx sender.pubKey "s"
          <> pubKeyLabels ctx recipient.pubKey "r"
      utxo =
        TxOut
          { value = fundingTxAmount,
            scriptPubKey = outputScript deployAddr,
            tokenData = Nothing
          }
  pure CmdContext {deployAddr = fromJust $ addrToText net deployAddr, ..}
  where
    err = error "Failed to load keys."

    network :: Bool -> Network
    network True = mainnet
    network False = chipnet
