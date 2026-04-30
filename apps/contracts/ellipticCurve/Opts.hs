-- Copyright (c) 2026 albaDsl

module Opts
  ( Opts (..),
    Command (..),
    DeployOpts (..),
    SpendOpts (..),
    ContractFunction (..),
    cmdOpts,
    execParser,
  )
where

import Alba.Misc.Opts
  ( DeployOpts (..),
    mainNetSwitch,
    recipientOption,
    txOption,
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
  = Deploy DeployOpts
  | Spend ContractFunction
  deriving (Show)

data SpendOpts = SpendOpts
  { mainNet :: Bool,
    txId :: String,
    recipient :: String
  }
  deriving (Show)

newtype ContractFunction
  = Withdraw SpendOpts
  deriving (Show)

cmdOpts :: ParserInfo Opts
cmdOpts =
  info
    (cmdOpts' <**> helper)
    ( fullDesc
        <> progDesc "Demonstration of EC scalar multiply in a contract."
    )

cmdOpts' :: Parser Opts
cmdOpts' =
  Opts
    <$> hsubparser
      ( command "deploy" (info deploy (progDesc "Display deploy data."))
          <> command
            "spend"
            (info spend (progDesc "Create Tx to spend contract UTXO."))
      )

deploy :: Parser Command
deploy = Deploy . DeployOpts <$> mainNetSwitch

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
        <$> mainNetSwitch
        <*> txOption
        <*> recipientOption
