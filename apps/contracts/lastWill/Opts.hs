-- Copyright (c) 2025 albaDsl

module Opts
  ( Opts (..),
    Command (..),
    DeployOpts (..),
    ContractFunction (..),
    RefreshOpts (..),
    SpendOpts (..),
    cmdOpts,
    execParser,
  )
where

import Alba.Misc.Opts
  ( DeployOpts (..),
    SpendOpts (..),
    mainNetSwitch,
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

data Command = Deploy DeployOpts | Spend ContractFunction | Test
  deriving (Show)

data ContractFunction
  = Refresh RefreshOpts
  | Withdraw SpendOpts
  | Inherit SpendOpts
  deriving (Show)

data RefreshOpts = RefreshOpts
  { txId :: String,
    utxoIndex :: Int,
    sim :: Bool,
    showMetrics :: Bool,
    mainNet :: Bool
  }
  deriving (Show)

cmdOpts :: ParserInfo Opts
cmdOpts =
  info
    (cmdOpts' <**> helper)
    (fullDesc <> progDesc "LastWill contract.")

cmdOpts' :: Parser Opts
cmdOpts' =
  Opts
    <$> hsubparser
      ( command "deploy" (info deploy (progDesc "Display deploy data."))
          <> command
            "spend"
            (info spend (progDesc "Create Tx to invoke contract function."))
          <> command "test" (info test (progDesc "Run tests."))
      )

deploy :: Parser Command
deploy = Deploy . DeployOpts <$> mainNetSwitch

spend :: Parser Command
spend =
  Spend
    <$> hsubparser
      ( command "refresh" (info refresh (progDesc "Keep contract alive."))
          <> command
            "withdraw"
            (info withdraw (progDesc "Withdraw funds from contract."))
          <> command
            "inherit"
            (info inherit (progDesc "Inherit funds from contract."))
      )
  where
    refresh =
      Refresh
        <$> ( RefreshOpts
                <$> txOption
                <*> utxoIndexOption
                <*> simSwitch
                <*> metricsSwitch
                <*> mainNetSwitch
            )

    withdraw = Withdraw <$> spendOpts

    inherit = Inherit <$> spendOpts

    spendOpts =
      SpendOpts
        <$> txOption
        <*> utxoIndexOption
        <*> recipientOption
        <*> simSwitch
        <*> metricsSwitch
        <*> mainNetSwitch

test :: Parser Command
test = pure Test
