-- Copyright (c) 2025 albaDsl

module Main (main) where

import Alba.Misc.Utils (decodeHex)
import Alba.Tx.Bch2025 (Tx (..), TxOut (..), TxOuts (..))
import Alba.Vm.Bch2025 (defaultDisplayOpts, mkTxContext, verifyScript)
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import Alba.Vm.Common (dumpVerifyScriptResult)
import Control.Applicative ((<**>))
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Options.Applicative
  ( Parser,
    ParserInfo (..),
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    option,
    progDesc,
    strOption,
    subparser,
  )
import Options.Applicative qualified as O

newtype Opts = Opts {command :: Command}

newtype Command = Vm VmOpts

data VmOpts = VmOpts
  { txHex :: String,
    utxosHex :: String,
    inputIndex :: Int
  }
  deriving (Show)

cmdOpts :: Parser Opts
cmdOpts =
  Opts
    <$> subparser
      ( O.command
          "vm"
          (info vm (progDesc "Evaluate script with VM."))
      )
  where
    vm :: Parser Command
    vm =
      Vm
        <$> ( VmOpts
                <$> strOption (long "tx" <> help "Transaction (hex).")
                <*> strOption (long "utxos" <> help "Source output (hex).")
                <*> option auto (long "input" <> help "Tx input to evaluate.")
            )

main :: IO ()
main = main' =<< execParser opts
  where
    opts :: ParserInfo Opts
    opts = info (cmdOpts <**> helper) (fullDesc <> progDesc "Alba.")

main' :: Opts -> IO ()
main' Opts {command = (Vm VmOpts {..})} = do
  let tx = decode (fromStrict (fromJust $ decodeHex $ T.pack txHex)) :: Tx
      txOuts =
        decode (fromStrict $ fromJust $ decodeHex $ T.pack utxosHex) :: TxOuts
      scriptPubKey = (txOuts.get !! inputIndex).scriptPubKey
      txContext = fromJust $ mkTxContext tx inputIndex txOuts.get
      displayOpts = defaultDisplayOpts
  dumpVerifyScriptResult
    displayOpts
    (verifyScript scriptPubKey txContext vmParamsStandard)
  pure ()
