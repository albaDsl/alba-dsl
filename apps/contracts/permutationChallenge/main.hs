-- Copyright (c) 2026 albaDsl

module Main (main) where

import Alba.Misc.Cmd (showTx)
import Alba.Misc.Haskoin
  ( Network,
    addrToText,
    chipnet,
    mainnet,
    pubKeyAddr,
    textToAddr,
    wrapPubKey,
  )
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Utils (canNotHappen)
import Alba.Misc.Wallet (getWallet)
import Alba.Tx.Bch2025 (OutPoint (..))
import Crypto.Secp256k1 (Ctx, withContext)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (pack)
import Deploy (deployTx)
import Opts
  ( Command (..),
    ContractFunction (..),
    DeployOpts (..),
    Opts (..),
    SpendOpts (..),
    WalletOpts (..),
    cmdOpts,
    execParser,
  )
import Params (contractAmount, deployAmount)
import Spend (withdrawTx)
import System.Environment (withArgs)
import Test (contractTests)
import Text.Printf (printf)

newtype CmdContext = CmdContext {net :: Network}

main :: IO ()
main = do
  opts <- execParser cmdOpts
  withContext (main' opts)

main' :: Opts -> Ctx -> IO ()
main' opts ctx =
  case opts.command of
    Wallet (WalletOpts {..}) -> do
      c <- cmdContext ctx mainNet
      KeyPair {..} <- fromMaybe err <$> getWallet c.net "alice"
      let recvAddr =
            fromMaybe
              canNotHappen
              (addrToText c.net (pubKeyAddr ctx (wrapPubKey False pubKey)))
      printf
        ( "\nFund the wallet with exactly %d satoshis. 10K will go to the\n"
            <> "permutation challenge contract. Rest to library UTXOs and\n"
            <> "fees. Addr: \n\n"
            <> "%s\n\n"
        )
        deployAmount
        recvAddr
      pure ()
    Deploy (DeployOpts {..}) -> do
      c <- cmdContext ctx mainNet
      let outPoint = OutPoint (fromString txId) (fromIntegral utxoIndex)
      tx <- deployTx ctx c.net outPoint
      showTx tx
    Spend (Withdraw (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let recvAddr = case textToAddr c.net (pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
          outPoint = OutPoint (fromString txId) (fromIntegral utxoIndex)
          dcTxId' = fromString dcTxId
          vcTxId' = fromString vcTxId
          tx = withdrawTx ctx outPoint contractAmount dcTxId' vcTxId' recvAddr
      showTx tx
    Test -> withArgs [] $ contractTests ctx
  where
    err = error "Failed to load keys."

cmdContext :: Ctx -> Bool -> IO CmdContext
cmdContext _ctx mainNetP = do
  let net = network mainNetP
  pure CmdContext {..}
  where
    network :: Bool -> Network
    network True = mainnet
    network False = chipnet
