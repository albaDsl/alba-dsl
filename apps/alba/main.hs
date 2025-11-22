-- Copyright (c) 2025 albaDsl

module Main (main) where

import Alba.Misc.Opts (metricsSwitch)
import Alba.Misc.Utils (decodeHex)
import Alba.Node.Validation (Mode (..), acceptToMemoryPool)
import Alba.Tx.Bch2025 (Tx (..), TxOut (..), TxOuts (..))
import Alba.Vm.Bch2025 (defaultDisplayOpts, mkTxContext)
import Alba.Vm.Bch2025 qualified as Vm2025
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import Alba.Vm.Common (dumpVerifyScriptResult)
import Alba.Vm.Common.Logging (LogDisplayOpts (..))
import Alba.Vm.Common.LoggingHtml (verifyScriptResultToHtml)
import Control.Applicative ((<**>))
import Control.Monad (when)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import LibauthSupport
  ( LibauthTest (..),
    TestMode (..),
    loadTests,
    testModeToVmSetup,
    testToTxAndUtxos,
  )
import Options.Applicative
  ( Parser,
    ParserInfo (..),
    auto,
    command,
    execParser,
    fullDesc,
    help,
    helper,
    hsubparser,
    info,
    long,
    option,
    progDesc,
    strOption,
    switch,
  )
import System.Exit (exitFailure)
import Text.Printf (printf)

newtype Opts = Opts {command :: Command}

data Command = Vm VmOpts | Libauth LibauthOpts

data VmOpts = VmOpts
  { txHex :: Text,
    utxosHex :: Text,
    inputIndex :: Int,
    metrics :: Bool
  }
  deriving (Show)

data LibauthOpts = LibauthOpts
  { file :: Text,
    shortId :: Text,
    testMode :: TestMode,
    metrics :: Bool,
    htmlLogs :: Bool
  }
  deriving (Show)

cmdOpts :: Parser Opts
cmdOpts =
  Opts
    <$> hsubparser
      ( command "vm" (info vm (progDesc "Evaluate script with VM."))
          <> command
            "libauth"
            (info libauth (progDesc "Evaluate Libauth test."))
      )
  where
    vm :: Parser Command
    vm =
      Vm
        <$> ( VmOpts
                <$> strOption (long "tx" <> help "Transaction (hex).")
                <*> strOption (long "utxos" <> help "Source output (hex).")
                <*> option auto (long "input" <> help "Tx input to evaluate.")
                <*> metricsSwitch
            )

    libauth :: Parser Command
    libauth =
      Libauth
        <$> ( LibauthOpts
                <$> strOption (long "file" <> help "vmb_tests.json filename.")
                <*> strOption (long "test" <> help "ShortId for the test.")
                <*> option auto (long "mode" <> help "Test mode.")
                <*> metricsSwitch
                <*> htmlLogsSwitch
            )

htmlLogsSwitch :: Parser Bool
htmlLogsSwitch = switch (long "html" <> help "Dump HTML logs to logs.html.")

main :: IO ()
main = main' =<< execParser opts
  where
    opts :: ParserInfo Opts
    opts = info (cmdOpts <**> helper) (fullDesc <> progDesc "Alba.")

main' :: Opts -> IO ()
main' Opts {command = (Vm VmOpts {..})} = do
  let tx = decode (fromStrict (fromJust $ decodeHex txHex)) :: Tx
      txOuts =
        decode (fromStrict $ fromJust $ decodeHex utxosHex) :: TxOuts
      scriptPubKey = (txOuts.get !! inputIndex).scriptPubKey
      txContext = fromJust $ mkTxContext tx inputIndex txOuts.get
      displayOpts = defaultDisplayOpts
  dumpVerifyScriptResult
    displayOpts
    (Vm2025.verifyScript scriptPubKey txContext vmParamsStandard)
  pure ()
main' Opts {command = (Libauth LibauthOpts {..})} = do
  tests <- loadTests (T.unpack file)
  case findTest shortId tests of
    Just test ->
      case testToTxAndUtxos test of
        Right (tx, txOuts) -> do
          let txContext = fromJust $ mkTxContext tx test.inputIndex txOuts.get
              (verifyScript, vmParams, mode) = testModeToVmSetup testMode
              validationResult =
                acceptToMemoryPool verifyScript txContext vmParams mode
          case validationResult of
            Right res ->
              let displayOpts = defaultDisplayOpts {showMetrics = metrics}
               in do
                    dumpVerifyScriptResult displayOpts res
                    when htmlLogs $
                      T.writeFile
                        "log.html"
                        (verifyScriptResultToHtml displayOpts res)
            Left err -> printf "VM evaluation failure: %s\n" (show err)
          printf "Libauth expected:\n"
          case mode of
            Standard -> do
              printf "Result: %s\n" $ show test.standardResult
              printf "Limits: %s\n" $ show test.standardLimits
            Nonstandard -> do
              printf "Result: %s\n" $ show test.nonstandardResult
              printf "Limits: %s\n" $ show test.nonstandardLimits
          printf "\n"
          pure ()
        Left _ -> error "Failure to decode test."
    Nothing -> do
      printf "Couldn't find test \"%s\" in file \"%s\".\n" shortId file
      exitFailure
  where
    findTest :: Text -> [LibauthTest] -> Maybe LibauthTest
    findTest _ [] = Nothing
    findTest shortId' (test : rest) =
      if shortId' == test.shortId
        then Just test
        else findTest shortId rest
