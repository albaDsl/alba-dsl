-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module LibauthSupport where

import Alba.Misc.Debug (traceShow)
import Alba.Misc.Utils (decodeHex)
import Alba.Node.Policy (solver)
import Alba.Node.Validation (Mode (..), VerifyScriptFun, acceptToMemoryPool)
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxOuts (..))
import Alba.Tx.Bch2025.TxOut (TxOut (..))
import Alba.Vm.Bch2025 qualified as Vm2025
import Alba.Vm.Bch2026 qualified as Vm2026
import Alba.Vm.Common
  ( LogDisplayOpts (..),
    ScriptError,
    VerifyScriptResult,
    VmParams,
    defaultDisplayOpts,
    dumpVerifyScriptResult,
    mkTxContext,
  )
import Alba.Vm.Common.OpcodeL2 (codeL1ToCodeL2)
import Alba.Vm.Common.VmState
  ( VerifyScriptResult (..),
    VmMetrics (..),
    VmState (..),
  )
import Data.Aeson qualified as A
import Data.Binary (decodeOrFail)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.FilePath (FilePath, (<.>), (</>))
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

data TestMode = Standard2025 | Nonstandard2025 | Standard2026 | Nonstandard2026
  deriving (Eq)

data LibAuthTest = LibAuthTest
  { testRecord :: LibAuthTestRecord,
    standardLimits :: Limits,
    nonstandardLimits :: Limits
  }

data LibAuthTestRecord = LibAuthTestRecord
  { shortId :: T.Text,
    testDescription :: T.Text,
    unlockingScriptAsm :: T.Text,
    redeemOrLockingScriptAsm :: T.Text,
    testTransactionHex :: T.Text,
    sourceOutputsHex :: T.Text,
    inputIndex :: Maybe Int
  }
  deriving (Show)

type LibAuthLimits = M.Map T.Text (Int, Int, Int, T.Text)

data Limits = Limits
  { densityControlLength :: Int,
    maxCost :: Int,
    cost :: Int
  }

type ResultOrFailure = Either TestLibauthFailure TestResult

data TestLibauthFailure = CanNotParseTx
  deriving (Show)

data TestResult = TestResult
  { test :: LibAuthTest,
    vmResult ::
      Either
        ValidationFailure
        (Either (ScriptError, VerifyScriptResult) VerifyScriptResult)
  }

instance A.FromJSON LibAuthTestRecord where
  parseJSON json = do
    A.Array arr <- pure json
    Just (A.String txt0) <- pure (arr V.!? 0)
    Just (A.String txt1) <- pure (arr V.!? 1)
    Just (A.String txt2) <- pure (arr V.!? 2)
    Just (A.String txt3) <- pure (arr V.!? 3)
    Just (A.String txt4) <- pure (arr V.!? 4)
    Just (A.String txt5) <- pure (arr V.!? 5)
    let res = arr V.!? 6
    let x = case res of
          Just (A.Number x') ->
            let Right x'' = (floatingOrInteger x' :: Either Double Int)
             in Just x''
          _ -> Nothing
    pure $ LibAuthTestRecord txt0 txt1 txt2 txt3 txt4 txt5 x

loadTests :: FilePath -> IO [LibAuthTest]
loadTests file = do
  let root = "." </> "test" </> "libauth"
      testFile = root </> file <.> "vmb_tests" <.> "json"
      standardLimitsFile = root </> file <.> "standard_limits" <.> "json"
      nonstandardLimitsFile = root </> file <.> "nonstandard_limits" <.> "json"
  tests <- A.eitherDecodeFileStrict testFile
  standardLimits <- A.eitherDecodeFileStrict standardLimitsFile
  nonstandardLimits <- A.eitherDecodeFileStrict nonstandardLimitsFile
  case (tests, standardLimits, nonstandardLimits) of
    (Right t, Right sl, Right nsl) -> pure $ toLibAuthTest sl nsl <$> t
    _ -> error "loadTests"

toLibAuthTest ::
  LibAuthLimits ->
  LibAuthLimits ->
  LibAuthTestRecord ->
  LibAuthTest
toLibAuthTest slMap nslMap testRecord@(LibAuthTestRecord {shortId}) =
  let standardLimits =
        let (densityControlLength, maxCost, cost, _) =
              fromMaybe (error "toLibAuthTest") (M.lookup shortId slMap)
         in Limits {..}
      nonstandardLimits =
        let (densityControlLength, maxCost, cost, _) =
              fromMaybe (error "toLibAuthTest") (M.lookup shortId nslMap)
         in Limits {..}
   in LibAuthTest {..}

printSummary :: [a] -> [b] -> IO ()
printSummary selectedTests allTests =
  if length selectedTests /= length allTests
    then
      printf
        "Running %d of %d tests\n"
        (length selectedTests)
        (length allTests)
    else
      printf "Running all %d tests\n" (length selectedTests)

runTest :: TestMode -> LibAuthTest -> IO (T.Text, ResultOrFailure)
runTest testMode test@(LibAuthTest {..}) = do
  let (verifyScript, vmParams, mode) = params testMode
  case txAndUtxos of
    Right (tx, txOuts) -> do
      let inputIndex = fromMaybe 0 testRecord.inputIndex
          txContext = fromJust $ mkTxContext tx inputIndex txOuts.get
          vmResult = acceptToMemoryPool verifyScript txContext vmParams mode
      -- pPrintLightBg test
      -- pPrintLightBg txOuts
      -- pPrintLightBg $ codeL1ToCodeL2 . (.scriptPubKey) <$> txOuts.get
      -- print $ codeL1ToCodeL2 . (.scriptPubKey) <$> txOuts.get
      -- print $ solver vmParams . (.scriptPubKey) <$> txOuts.get
      -- pPrintLightBg tx
      pure (testRecord.shortId, Right $ TestResult {..})
    Left err -> pure (testRecord.shortId, Left err)
  where
    params :: TestMode -> (VerifyScriptFun, VmParams, Mode)
    params Standard2025 =
      (Vm2025.verifyScript, Vm2025.vmParamsStandard, Standard)
    params Nonstandard2025 =
      (Vm2025.verifyScript, Vm2025.vmParamsNonStandard, NonStandard)
    params Standard2026 =
      (Vm2026.verifyScript, Vm2026.vmParamsStandard, Standard)
    params Nonstandard2026 =
      (Vm2026.verifyScript, Vm2026.vmParamsNonStandard, NonStandard)

    txAndUtxos :: Either TestLibauthFailure (Tx, TxOuts)
    txAndUtxos = do
      tx <-
        either
          (\(_, _, _) -> Left CanNotParseTx)
          (\(_, _, res) -> Right res)
          ( decodeOrFail
              ( B.fromStrict $
                  fromJust $
                    decodeHex testRecord.testTransactionHex
              )
          )
      txOuts <-
        either
          (\(_, _, _) -> Left CanNotParseTx)
          (\(_, _, res) -> Right res)
          ( decodeOrFail
              (B.fromStrict $ fromJust $ decodeHex testRecord.sourceOutputsHex)
          )
      pure (tx, txOuts)

verifyTxApproved :: TestMode -> (T.Text, ResultOrFailure) -> IO ()
verifyTxApproved testMode (testId, res) =
  case res of
    Right testResult ->
      case testResult.vmResult of
        (Left err) -> do
          assertFailure (printf "%s: validation failure %s" testId (show err))
        (Right res'@(Left (err, _))) -> do
          let displayOpts = defaultDisplayOpts {showMetrics = True}
          dumpVerifyScriptResult displayOpts res'
          assertFailure (printf "%s: failed with %s" testId (show err))
        (Right (Right _res')) -> do
          -- dumpVerifyScriptResult Nothing True res'
          assertBool "metrics" (verifyMetrics testMode testResult)
          pure ()
    Left err ->
      assertFailure (printf "%s: failed with %s" testId (show err))

verifyTxNotApproved :: TestMode -> (T.Text, ResultOrFailure) -> IO ()
verifyTxNotApproved _testMode (testId, res) =
  case res of
    Right testResult ->
      case testResult.vmResult of
        (Left _err) -> pure ()
        (Right (Left (_err, _))) -> pure ()
        (Right _res'@(Right _)) -> do
          -- let displayOpts = defaultDisplayOpts {showMetrics = True}
          -- dumpVerifyScriptResult displayOpts res'
          assertFailure (printf "%s: passed validation" testId)
    Left _ -> pure ()

verifyMetrics :: TestMode -> TestResult -> Bool
verifyMetrics testMode TestResult {test = LibAuthTest {..}, ..} =
  let standard = case testMode of
        Standard2025 -> True
        Nonstandard2025 -> False
        Standard2026 -> True
        Nonstandard2026 -> False
      limits = if standard then standardLimits else nonstandardLimits
      verifyScriptResult = case vmResult of
        Right (Right (VerifyScriptResult {..})) -> scriptRedeemResult
        Right (Left (_, VerifyScriptResult {..})) -> scriptRedeemResult
        _ -> Nothing
   in case verifyScriptResult of
        Just (VmState {metrics = VmMetrics {cost}}) ->
          let res = cost == limits.cost
           in if res
                then res
                else traceShow (testRecord.shortId, cost, limits.cost) res
        Nothing -> True

tryTest :: TestMode -> LibAuthTest -> IO ()
tryTest testMode t@(LibAuthTest {testRecord}) = do
  (testId, res) <- runTest testMode t
  case res of
    Right testResult ->
      case testResult.vmResult of
        (Left err) -> do
          printf " , \"%s\" -- validation failure %s\n" testId (show err)
        (Right (Left (err, _))) -> do
          printf " , \"%s\" -- failed with %s\n" testId (show err)
        (Right (Right _res')) -> do
          printf " , \"%s\" -- passed validation.\n" testRecord.shortId
    Left err ->
      printf " , \"%s\" -- failure %s\n" testId (show err)
  1 @?= (1 :: Int)
