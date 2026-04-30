-- Copyright (c) 2026 albaDsl

module Main (main) where

import Alba.Dsl.V1.Bch2026 (CodeL1)
import Alba.Misc.Bchn (postTx)
import Alba.Misc.Cmd (askUser, deployMsg, showTx)
import Alba.Misc.Haskoin
  ( Network,
    addrToText,
    chipnet,
    mainnet,
    textToAddr,
  )
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx, withContext)
import Data.ByteString qualified as B
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

data CmdContext = CmdContext
  { net :: Network,
    redeemScript :: CodeL1,
    deployAddr :: Text
  }

main :: IO ()
main = do
  opts <- execParser cmdOpts
  withContext (main' opts)

main' :: Opts -> Ctx -> IO ()
main' opts ctx =
  case opts.command of
    Deploy (DeployOpts {..}) -> do
      c <- cmdContext ctx mainNet
      deployMsg fundingTxAmount c.deployAddr (Just (B.length c.redeemScript))
    Spend (Withdraw (SpendOpts {..})) -> do
      c <- cmdContext ctx mainNet
      let recvAddr = case textToAddr c.net (pack recipient) of
            Just x -> x
            Nothing -> error "Invalid recipient."
      tx <- withdrawTx ctx c.net (fromString txId) c.redeemScript recvAddr
      showTx tx
      txId' <-
        askUser "Spend the contract funds using this transaction?" >>= \case
          True -> postTx c.net tx
          False -> pure $ Left "Aborted."
      either print print txId'
      pure ()

cmdContext :: Ctx -> Bool -> IO CmdContext
cmdContext _ctx mainNetP = do
  let net = network mainNetP
  (redeemScript, deployAddr) <- instantiate
  pure CmdContext {deployAddr = fromJust $ addrToText net deployAddr, ..}
  where
    network :: Bool -> Network
    network True = mainnet
    network False = chipnet
