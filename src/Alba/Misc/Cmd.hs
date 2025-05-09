-- Copyright (c) 2025 albaDsl

module Alba.Misc.Cmd
  ( deployMsg,
    barcodeShowInTerminal,
    showTx,
    runInVm,
  )
where

import Alba.Misc.Utils (encodeHex)
import Alba.Tx.Bch2025 (Tx (..), TxOut (..))
import Alba.Vm.Bch2025
  ( LogDisplayOpts,
    dumpVerifyScriptResult,
    mkTxContext,
    verifyScript,
  )
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import Control.Monad (void, when)
import Data.Binary (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as T
import Data.Word (Word64)
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)
import Turtle.Bytes (shell)
import Turtle.Prelude (which)

deployMsg :: Word64 -> Text -> Maybe Int -> IO ()
deployMsg amount address size = do
  printf
    ( "\n"
        <> "Address to fund is below. Before funding, make sure the\n"
        <> "contract parameters are correct, otherwise update and\n"
        <> "re-run to generate a new address.\n\n"
        <> "Amount to fund: %d\n\n"
        <> "Contract address:\n\n%s\n\n"
    )
    amount
    address
  barcodeShowInTerminal address
  for_ size (printf "Contract size: %d bytes.\n\n")

barcodeShowInTerminal :: Text -> IO ()
barcodeShowInTerminal addr = do
  let qrencode = "qrencode" :: String
      kitten = "kitten" :: String
      cmd = pack $ printf "%s -s 15 -o - | %s icat --align left" qrencode kitten
  res1 <- which qrencode
  res2 <- which kitten
  when (isJust res1 && isJust res2) $
    void (shell cmd (pure $ T.encodeUtf8 addr))

showTx :: Tx -> IO ()
showTx tx = do
  let txBin = toStrict (encode tx)
  pPrintLightBg tx
  printf "\n\n"
  printf "Tx hex:\n\n%v\n\n" (encodeHex txBin)

runInVm :: LogDisplayOpts -> TxOut -> Tx -> IO ()
runInVm displayOpts utxo tx = do
  let context = fromMaybe (error "runInVm") $ mkTxContext tx 0 [utxo]
  dumpVerifyScriptResult
    displayOpts
    (verifyScript utxo.scriptPubKey context vmParamsStandard)
