-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TestUtils where

import Alba.Dsl.V1.Bch2025
  ( CompilationResult (..),
    FNA,
    FunctionTable,
    Optimize (..),
    compile,
    outputScript,
    writeFunctionTable,
  )
import Alba.Dsl.V1.Common.FunctionTableJson
  ( FunctionTableEntry (..),
    tableEntries,
  )
import Alba.Misc.Logging qualified as ML
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025 qualified as Bch2025
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.BchSpec qualified as BchSpec
import Alba.Vm.Common
  ( CodeL1,
    ScriptError,
    TxContext,
    VmMetrics,
    VmStack,
    VmState (..),
    mkTxContext,
  )
import Alba.Vm.Common.Logging (defaultDisplayOpts)
import Alba.Vm.Common.Logging qualified as Log
import Alba.Vm.Common.LoggingHtml (logDataToHtml)
import Alba.Vm.Common.LoggingText (logDataToText)
import Alba.Vm.Common.StackElement (i2SeUnsafe)
import Alba.Vm.Common.VmState (VmLogs)
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace (trace)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

data TestResult = TestResult
  { s :: !VmStack,
    alt :: !VmStack,
    metrics :: !VmMetrics,
    limits :: !VmMetrics,
    logData :: !(Maybe VmLogs),
    compilationResult :: !(Maybe CompilationResult)
  }
  deriving (Eq, Show)

isTrue :: Either (ScriptError, Maybe TestResult) TestResult -> Assertion
isTrue res =
  case res of
    Right tr -> do
      -- dumpLogToFile tr
      -- case tr.compilationResult of
      --   Just r -> writeFunctionTable r.code r.functionTable
      --   Nothing -> pure ()
      (tr.s, tr.alt) @?= (S.fromList [i2SeUnsafe 1], S.empty)
    Left (err, Just tr) -> do
      dumpLogToFile tr
      showLog tr id $ assertFailure ("isTrue: " <> show err)
    Left (err, Nothing) -> assertFailure ("isTrue: " <> show err)

isTrue' :: Either (ScriptError, Maybe TestResult) TestResult -> Bool
isTrue' res =
  case res of
    Right tr -> (tr.s, tr.alt) == (S.fromList [i2SeUnsafe 1], S.empty)
    Left (err, Just tr) ->
      showLog tr id $ error ("isTrue': " <> show err)
    Left (err, Nothing) -> error ("isTrue': " <> show err)

isErr ::
  Either (ScriptError, Maybe TestResult) TestResult ->
  ScriptError ->
  Assertion
isErr res err =
  case res of
    Right tr -> showLog tr id $ assertFailure "isErr: successful result."
    Left (err', _) -> err' @?= err

getStack :: Either (ScriptError, Maybe TestResult) TestResult -> VmStack
getStack res =
  case res of
    Right tr -> tr.s
    Left (err, Just tr) -> showLog tr $ error ("getStack: " <> show err)
    Left (err, Nothing) -> error ("getStack: " <> show err)

getStacks ::
  Either (ScriptError, Maybe TestResult) TestResult -> (VmStack, VmStack)
getStacks res =
  case res of
    Right tr -> (tr.s, tr.alt)
    Left (err, Just tr) -> showLog tr $ error ("getStacks: " <> show err)
    Left (err, Nothing) -> error ("getStacks: " <> show err)

getErr ::
  Either (ScriptError, Maybe TestResult) TestResult -> ScriptError
getErr res =
  case res of
    Right tr -> showLog tr $ error "getErr: expected an error."
    Left (err, _) -> err

showLog :: TestResult -> a -> a
showLog tr =
  trace (T.unpack $ logDataToText defaultDisplayOpts tr.logData)

dumpLogToFile :: TestResult -> IO ()
dumpLogToFile TestResult {..} =
  ML.dumpLogToFile compilationResult logData "log.html"

evaluateProg ::
  FNA s '[] s' alt' ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateProg prog = evaluateProgWithStack prog (S.empty, S.empty)

evaluateProgWithStack ::
  FNA s '[] s' alt' ->
  (VmStack, VmStack) ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateProgWithStack prog (s, alt) =
  evaluateScript (compile None prog) (s, alt) minimalContext

evaluateScript ::
  CodeL1 ->
  (VmStack, VmStack) ->
  TxContext ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateScript code (s, alt) context = do
  let res2025 =
        let state = (Bch2025.startState Bch2025.vmParamsStandard) {code, s, alt}
         in toTestResult $ Bch2025.evaluateScript context state
      res2026 =
        let state = (Bch2026.startState Bch2026.vmParamsStandard) {code, s, alt}
         in toTestResult $ Bch2026.evaluateScript context state
      resSpec =
        let state = (BchSpec.startState BchSpec.vmParamsStandard) {code, s, alt}
         in toTestResult $ BchSpec.evaluateScript context state
  unless (res2025 == res2026 && res2026 == resSpec) $
    error "Bch2025 / Bch2026 / BchSpec results don't match."
  res2026

toTestResult ::
  Either (ScriptError, Maybe VmState) VmState ->
  Either (ScriptError, Maybe TestResult) TestResult
toTestResult res =
  case res of
    Right st -> Right (convert st)
    Left (err, st) -> Left (err, convert <$> st)
  where
    convert VmState {s, alt, metrics, limits, logData} =
      TestResult {compilationResult = Nothing, ..}

minimalContext :: TxContext
minimalContext = fromJust $ mkTxContext barboneTx 0 undefined

barboneTx :: Tx
barboneTx =
  Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}

txContext :: TxOut -> TxContext
txContext utxo = fromJust $ mkTxContext tx 0 [utxo]

tx :: Tx
tx =
  Tx
    { version = 2,
      inputs =
        [ TxIn
            { prevout = OutPoint {txId = mockTxId, index = 0},
              scriptSig = [],
              sequence = 0
            }
        ],
      outputs =
        [ TxOut
            { value = 10_000,
              scriptPubKey = scriptPubKey,
              tokenData = Nothing
            }
        ],
      lockTime = 0
    }
  where
    scriptPubKey :: B.ByteString
    scriptPubKey = outputScript mockAddr

utxoWithPubkey :: B.ByteString -> TxOut
utxoWithPubkey scriptPubKey =
  TxOut
    { value = 10_000,
      scriptPubKey = scriptPubKey,
      tokenData = Nothing
    }
