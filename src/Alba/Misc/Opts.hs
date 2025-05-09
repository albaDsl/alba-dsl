-- Copyright (c) 2025 albaDsl

module Alba.Misc.Opts
  ( SpendOpts (..),
    DeployOpts (..),
    txOption,
    utxoIndexOption,
    recipientOption,
    simSwitch,
    metricsSwitch,
    mainNetSwitch,
  )
where

import Options.Applicative
  ( Parser,
    auto,
    help,
    long,
    metavar,
    option,
    short,
    showDefault,
    strOption,
    switch,
    value,
  )

data SpendOpts = SpendOpts
  { txId :: String,
    utxoIndex :: Int,
    recipient :: String,
    sim :: Bool,
    showMetrics :: Bool,
    mainNet :: Bool
  }
  deriving (Show)

newtype DeployOpts = DeployOpts {mainNet :: Bool}
  deriving (Show)

txOption :: Parser String
txOption =
  strOption
    ( long "tx"
        <> short 't'
        <> metavar "TxId"
        <> help "Id of tx with UTXO to spend."
    )

utxoIndexOption :: Parser Int
utxoIndexOption =
  option
    auto
    ( long "utxoIndex"
        <> short 'i'
        <> metavar "index"
        <> value (0 :: Int)
        <> showDefault
        <> help "Index of the UTXO in the transaction."
    )

recipientOption :: Parser String
recipientOption =
  strOption
    ( long "recipient"
        <> short 'r'
        <> metavar "ADDRESS"
        <> help "Where to send the funds."
    )

simSwitch :: Parser Bool
simSwitch = switch (short 's' <> help "Simulate the function invocation.")

metricsSwitch :: Parser Bool
metricsSwitch = switch (short 'm' <> help "Show metric details in simulation.")

mainNetSwitch :: Parser Bool
mainNetSwitch =
  switch (long "live" <> help "Use the live net (mainnet) instead of chipnet.")
