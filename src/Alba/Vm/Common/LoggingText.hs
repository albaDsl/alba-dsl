-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.LoggingText
  ( dumpLog,
    logDataToText,
    dumpVerifyScriptResult,
    formatStack,
    formatOp,
    formatMetrics,
  )
where

import Alba.Misc.Debug (printf)
import Alba.Misc.Utils (encodeHex)
import Alba.Vm.Common.Logging (LogDisplayOpts (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.StackElement (Labels, showStackElement)
import Alba.Vm.Common.Utils (formatBytesWithLabels)
import Alba.Vm.Common.VmLimits (dumpMetrics)
import Alba.Vm.Common.VmStack (VmStack)
import Alba.Vm.Common.VmState
  ( LogEntry (..),
    Operation (..),
    VerifyScriptResult (..),
    VmLogs,
    VmMetrics (..),
    VmState (..),
  )
import Control.Monad (when)
import Data.ByteString qualified as B
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Prelude hiding (log)

dumpLog :: LogDisplayOpts -> VmState -> IO ()
dumpLog displayOpts VmState {logData} = do
  printf "%s\n" (logDataToText displayOpts logData)

logDataToText :: LogDisplayOpts -> Maybe VmLogs -> T.Text
logDataToText _ Nothing = "No logs."
logDataToText displayOpts (Just logData) =
  T.concat $ logEntryLine displayOpts <$> toList logData

logEntryLine :: LogDisplayOpts -> LogEntry -> T.Text
logEntryLine LogDisplayOpts {..} (Completed {..}) =
  let opStr =
        case op of
          Op op' -> formatOp labels op'
          Start -> "(Start Stack)"
          FunctionExit -> "(Function Exit)"
      stack' = formatStack labels stack
      metrics' = formatMetrics metrics
   in case (exec || op == Start, showMetrics) of
        (True, True) ->
          T.pack $
            printf " %-30s | %-20s | %s\n" opStr metrics' stack'
        (True, False) ->
          T.pack $ printf " %-30s | %s\n" opStr stack'
        _ -> T.empty
logEntryLine LogDisplayOpts {..} (Failed {..}) =
  let (opStr, execStr :: T.Text) = (formatOp labels opcode, "+")
   in T.pack $ printf "%s %-30s | (operation failed)\n" execStr opStr

formatOp :: Maybe Labels -> OpcodeL2 -> T.Text
formatOp labels op =
  case op of
    OP_DATA opcodeL1 bytes ->
      T.pack (show opcodeL1) <> " " <> formatBytesWithLabels labels bytes
    _ -> T.pack $ show op

formatStack :: Maybe Labels -> VmStack -> T.Text
formatStack labels s =
  Data.List.foldl'
    (\a x -> a <> T.pack (printf " %s" x))
    ("" :: T.Text)
    (showStackElement labels <$> s)

formatMetrics :: VmMetrics -> T.Text
formatMetrics VmMetrics {..} =
  T.pack $
    printf
      "c:%5d i:%2d b:%4d a:%4d h:%2d s:%d"
      cost
      instructions
      pushedBytes
      arithmeticBytes
      hashIterations
      sigChecks

dumpVerifyScriptResult ::
  LogDisplayOpts ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult ->
  IO ()
dumpVerifyScriptResult displayOpts@LogDisplayOpts {showMetrics} result = do
  let (VerifyScriptResult {..}, msg) = case result of
        (Right res) -> (res, "Successful script verification.\n\n")
        (Left (scriptError, res)) ->
          ( res,
            printf "Script verification failed with: %s\n\n" (show scriptError)
          )
  showLabels displayOpts.labels
  case scriptSigResult of
    Just res -> do
      printf "scriptSig:\n"
      dumpLog displayOpts res
    Nothing -> pure ()

  case scriptPubKeyResult of
    Just res -> do
      printf "scriptPubKey:\n"
      dumpLog displayOpts res
      when (showMetrics && isNothing scriptRedeemResult) $
        dumpMetrics res
    Nothing -> pure ()

  case scriptRedeemResult of
    Just res -> do
      printf "redeemScript:\n"
      dumpLog displayOpts res
      when showMetrics $ dumpMetrics res
    Nothing -> pure ()
  printf msg

showLabels :: Maybe Labels -> IO ()
showLabels (Just labels) = do
  printf "Labels: \n"
  mapM_ showLabel (M.toList labels)
  printf "\n"
  where
    showLabel :: (B.ByteString, T.Text) -> IO ()
    showLabel (bs, name) =
      printf " %s: %s (%d)\n" name (encodeHex bs) (B.length bs)
showLabels Nothing = pure ()
