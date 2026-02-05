-- Copyright (c) 2026 albaDsl

module Opts
  ( Opts (..),
    Command (..),
    WalletOpts (..),
    DeployOpts (..),
    SpendOpts (..),
    ContractFunction (..),
    cmdOpts,
    execParser,
  )
where

import Alba.Misc.Opts
  ( mainNetSwitch,
    metricsSwitch,
    recipientOption,
    simSwitch,
    txOption,
    utxoIndexOption,
  )
import Control.Applicative ((<**>))
import Options.Applicative
  ( Parser,
    ParserInfo (..),
    command,
    execParser,
    fullDesc,
    helper,
    hsubparser,
    info,
    progDesc,
  )

newtype Opts = Opts {command :: Command}
  deriving (Show)

data Command
  = Wallet WalletOpts
  | Deploy DeployOpts
  | Spend ContractFunction
  | Test
  deriving (Show)

newtype WalletOpts = WalletOpts {mainNet :: Bool} deriving (Show)

data DeployOpts = DeployOpts
  { mainNet :: Bool,
    txId :: String,
    utxoIndex :: Int
  }
  deriving (Show)

data SpendOpts = SpendOpts
  { txId :: String,
    recipient :: String,
    sim :: Bool,
    showMetrics :: Bool,
    mainNet :: Bool
  }
  deriving (Show)

newtype ContractFunction
  = Withdraw SpendOpts
  deriving (Show)

cmdOpts :: ParserInfo Opts
cmdOpts =
  info
    (cmdOpts' <**> helper)
    (fullDesc <> progDesc "PermutationChallenge contract.")

cmdOpts' :: Parser Opts
cmdOpts' =
  Opts
    <$> hsubparser
      ( command "wallet" (info wallet (progDesc walletMsg))
          <> command "deploy" (info deploy (progDesc deployMsg))
          <> command
            "spend"
            (info spend (progDesc "Create Tx to invoke contract function."))
          <> command "test" (info test (progDesc "Run tests."))
      )
  where
    walletMsg = "Show wallet details to allow it to be funded."
    deployMsg = "Specify UTXO to fund the contract from."

wallet :: Parser Command
wallet = Wallet . WalletOpts <$> mainNetSwitch

deploy :: Parser Command
deploy =
  Deploy
    <$> ( DeployOpts
            <$> mainNetSwitch
            <*> txOption
            <*> utxoIndexOption
        )

spend :: Parser Command
spend =
  Spend
    <$> hsubparser
      ( command
          "withdraw"
          (info withdraw (progDesc "Withdraw funds from contract."))
      )
  where
    withdraw = Withdraw <$> spendOpts

    spendOpts =
      SpendOpts
        <$> txOption
        <*> recipientOption
        <*> simSwitch
        <*> metricsSwitch
        <*> mainNetSwitch

test :: Parser Command
test = pure Test
