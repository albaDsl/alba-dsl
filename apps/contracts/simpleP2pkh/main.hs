-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    FN,
    Optimize (None),
    TPubKey,
    TSig,
    bytes',
    compile,
    outputScript,
    setScriptSig,
    signAll,
    (#),
    type (>),
  )
import Alba.Misc.Cmd (deployMsg, runInVm, showTx)
import Alba.Misc.Haskoin
  ( Address,
    Network,
    addrToText,
    chipnet,
    mainnet,
    marshal,
    pubKeyAddr,
    textToAddr,
    wrapPubKey,
  )
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Opts (DeployOpts (..))
import Alba.Misc.Wallet (getWallet)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxId, TxIn (..), TxOut (..))
import Alba.Vm.Bch2025 (Labels, LogDisplayOpts (..), defaultDisplayOpts)
import Alba.Vm.Common.Utils (pubKeyLabels)
import Crypto.Secp256k1 (Ctx, withContext)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Opts (Command (..), Opts (..), SpendOpts (..), cmdOpts, execParser)

fundingTxAmount :: Word64
fundingTxAmount = 10_000

main :: IO ()
main = do
  opts <- execParser cmdOpts
  withContext (main' opts)

data CmdContext = CmdContext
  { alice :: KeyPair,
    deployAddr :: Text,
    utxo :: TxOut,
    labels :: Labels,
    net :: Network
  }

main' :: Opts -> Ctx -> IO ()
main' opts ctx = do
  case opts.command of
    Deploy (DeployOpts {..}) -> do
      c <- cmdContext ctx mainNet
      deployMsg fundingTxAmount c.deployAddr Nothing
    Spend (SpendOpts {..}) -> do
      c <- cmdContext ctx mainNet
      let txId' = fromString txId :: TxId
          recvAddr = case textToAddr c.net (T.pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          tx = txTemplate txId' recvAddr
          sig = signAll ctx tx c.utxo.scriptPubKey c.utxo 0 c.alice.secKey
          sig' = marshal ctx sig
          pubKey' = marshal ctx (wrapPubKey False c.alice.pubKey)
          tx' = setScriptSig 0 (compile None (scriptSig sig' pubKey')) tx
          labels' = Just $ M.fromList [(marshal ctx sig, "sig")] <> c.labels
          displayOpts = defaultDisplayOpts {Alba.Vm.Bch2025.labels = labels'}
      if sim then runInVm displayOpts c.utxo tx' else showTx tx'

cmdContext :: Ctx -> Bool -> IO CmdContext
cmdContext ctx mainNetP = do
  let net = network mainNetP
  alice@KeyPair {..} <- fromMaybe err <$> getWallet net "alice"
  let deployAddr = pubKeyAddr ctx (wrapPubKey False pubKey)
      utxo =
        TxOut
          { value = fundingTxAmount,
            scriptPubKey = outputScript deployAddr,
            tokenData = Nothing
          }
      labels = pubKeyLabels ctx alice.pubKey "alice"
  pure CmdContext {deployAddr = fromJust $ addrToText net deployAddr, ..}
  where
    err = error "Failed to load keys."

    network :: Bool -> Network
    network True = mainnet
    network False = chipnet

txTemplate :: TxId -> Address -> Tx
txTemplate fundingTx recvAddr =
  Tx
    { version = 2,
      inputs =
        [ TxIn
            { prevout = OutPoint {txId = fundingTx, index = 0},
              scriptSig = [],
              sequence = 0
            }
        ],
      outputs =
        [ TxOut
            { value = 9_500,
              scriptPubKey = outputScript recvAddr,
              tokenData = Nothing
            }
        ],
      lockTime = 0
    }

scriptSig :: Bytes -> Bytes -> FN s (s > TPubKey > TSig)
scriptSig pubKey sig = bytes' pubKey # bytes' sig
