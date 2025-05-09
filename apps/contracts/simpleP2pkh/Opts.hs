-- Copyright (c) 2025 albaDsl

module Opts
  ( Opts (..),
    Command (..),
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

data Command = Deploy DeployOpts | Spend SpendOpts
  deriving (Show)

cmdOpts :: ParserInfo Opts
cmdOpts = info (cmdOpts' <**> helper) (fullDesc <> progDesc "Simple P2PKH.")

cmdOpts' :: Parser Opts
cmdOpts' =
  Opts
    <$> hsubparser
      ( command "deploy" (info deploy (progDesc "Display deploy data."))
          <> command
            "spend"
            (info spend (progDesc "Create Tx to spend the funds."))
      )

deploy :: Parser Command
deploy = Deploy . DeployOpts <$> mainNetSwitch

spend :: Parser Command
spend =
  Spend
    <$> ( SpendOpts
            <$> txOption
            <*> utxoIndexOption
            <*> recipientOption
            <*> simSwitch
            <*> metricsSwitch
            <*> mainNetSwitch
        )
