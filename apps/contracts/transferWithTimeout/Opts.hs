-- Copyright (c) 2025 albaDsl

module Opts
  ( Opts (..),
    Command (..),
    DeployOpts (..),
    ContractFunction (..),
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

data ContractFunction = SenderWithdraw SpendOpts | RecipientWithdraw SpendOpts
  deriving (Show)

cmdOpts :: ParserInfo Opts
cmdOpts =
  info
    (cmdOpts' <**> helper)
    (fullDesc <> progDesc "TransferWithTimeout contract.")

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
      ( command
          "senderWithdraw"
          (info sender (progDesc "Sender withdrawal of funds after timeout."))
          <> command
            "recipientWithdraw"
            (info recipient (progDesc "Recipient withdrawal of funds."))
      )
  where
    sender = SenderWithdraw <$> spendOpts

    recipient = RecipientWithdraw <$> spendOpts

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
