-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.Logging
  ( LogDisplayOpts (..),
    defaultDisplayOpts,
    logOp,
    dumpLog,
    dumpVerifyScriptResult,
  )
where

import Alba.Misc.Debug (printf)
import Alba.Misc.Utils (encodeHex)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.StackElement (Labels, showStackElement)
import Alba.Vm.Common.Utils (formatBytesWithLabels)
import Alba.Vm.Common.VmLimits (dumpLimits, dumpMetrics)
import Alba.Vm.Common.VmState
  ( LogEntry (..),
    VerifyScriptResult (..),
    VmLogs,
    VmMetrics (..),
    VmStack,
    VmState (..),
  )
import Control.Monad (when)
import Data.ByteString qualified as B
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Prelude hiding (log)

data LogDisplayOpts = LogDisplayOpts
  { labels :: Maybe Labels,
    showMetrics :: Bool,
    showUnexecuted :: Bool
  }

defaultDisplayOpts :: LogDisplayOpts
defaultDisplayOpts =
  LogDisplayOpts
    { labels = Nothing,
      showMetrics = False,
      showUnexecuted = False
    }

logOp :: Maybe OpcodeL2 -> Bool -> VmState -> VmState
logOp _op _exec state@VmState {logData = Nothing} = state
logOp op exec state@VmState {s, alt, metrics, logData = Just logData} =
  let entry = LogEntry {stack = s, altStack = alt, ..}
   in state {logData = Just $ logData S.|> entry}

dumpLog :: LogDisplayOpts -> VmState -> IO ()
dumpLog displayOpts VmState {logData} = do
  let log = foldl' (<>) "" (logDataToText displayOpts logData)
  printf "%s\n" log

logDataToText :: LogDisplayOpts -> Maybe VmLogs -> [T.Text]
logDataToText _ Nothing = ["No logs."]
logDataToText displayOpts (Just logData) =
  logEntryLine displayOpts <$> toList logData

logEntryLine :: LogDisplayOpts -> LogEntry -> T.Text
logEntryLine LogDisplayOpts {..} LogEntry {..} =
  let (opStr, execStr :: T.Text) =
        case op of
          Just op' -> (formatOp labels op', if exec then "+" else "-")
          Nothing -> ("", " ")
      stack' = formatStack labels stack
      metrics' = formatMetrics metrics
   in case (showUnexecuted || exec || isNothing op, showMetrics) of
        (True, True) ->
          T.pack $
            printf "%s %-30s | %-20s | %s\n" execStr opStr metrics' stack'
        (True, False) ->
          T.pack $ printf "%s %-30s | %s\n" execStr opStr stack'
        _ -> T.empty

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
  Either
    (ScriptError, VerifyScriptResult)
    VerifyScriptResult ->
  IO ()
dumpVerifyScriptResult displayOpts (Right res) = do
  dumpVerifyScriptResult' displayOpts res
  printf "Successful script verification.\n\n"
dumpVerifyScriptResult displayOpts (Left (scriptError, state)) = do
  dumpVerifyScriptResult' displayOpts state
  printf "Script verification failed with: %s\n\n" (show scriptError)

dumpVerifyScriptResult' :: LogDisplayOpts -> VerifyScriptResult -> IO ()
dumpVerifyScriptResult'
  displayOpts@LogDisplayOpts {showMetrics}
  VerifyScriptResult {..} = do
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
          dumpMetricsAndLimits res
      Nothing -> pure ()

    case scriptRedeemResult of
      Just res -> do
        printf "redeemScript:\n"
        dumpLog displayOpts res
        when showMetrics $ dumpMetricsAndLimits res
      Nothing -> pure ()
    where
      dumpMetricsAndLimits res' = dumpMetrics res' >> dumpLimits res'

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
