-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module LibauthSupport
  ( findAndLoad,
    runTest,
    tryTest,
    printSummary,
    verifyResult,
    LibauthTest (..),
    TestMode (..),
    loadTests,
    testModeToVmSetup,
    testToTxAndUtxos,
  )
where

import Alba.Misc.Debug (traceShow)
import Alba.Misc.Utils (canNotHappen, decodeHex, encodeHex)
import Alba.Node.Policy (solver)
import Alba.Node.Validation
  ( AcceptToMemoryPoolResult,
    Mode (..),
    VerifyScriptFun,
    acceptToMemoryPool,
  )
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxOuts (..))
import Alba.Tx.Bch2025.TxIn (TxIn (..))
import Alba.Tx.Bch2025.TxOut (TxOut (..))
import Alba.Vm.Bch2025 qualified as Vm2025
import Alba.Vm.Bch2026 qualified as Vm2026
import Alba.Vm.BchSpec qualified as VmSpec
import Alba.Vm.Common
  ( LogDisplayOpts (..),
    ScriptError (..),
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
import Control.Monad (unless)
import Data.Aeson qualified as A
import Data.Binary (decodeOrFail)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import LibauthFileParse
  ( LibAuthFileLimits,
    LibAuthFileResults,
    LibAuthFileVmbTestsRecord (..),
  )
import System.Directory (listDirectory)
import System.FilePath (FilePath, takeDirectory, (<.>), (</>))
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

data TestMode
  = Standard2025
  | Nonstandard2025
  | Standard2026
  | Nonstandard2026
  | StandardSpec
  | NonstandardSpec
  deriving (Eq, Show, Read)

data LibauthTest = LibauthTest
  { shortId :: Text,
    testClass :: Text,
    inputIndex :: Int,
    testTransactionHex :: Text,
    sourceOutputsHex :: Text,
    standardResult :: ExpectedResult,
    nonstandardResult :: ExpectedResult,
    standardLimits :: Limits,
    nonstandardLimits :: Limits
  }
  deriving (Show)

data ExpectedResult = Success | Err T.Text
  deriving (Show)

data Limits = Limits
  { densityControlLength :: Int,
    maxCost :: Int,
    cost :: Int
  }
  deriving (Show)

type ResultOrSetupFailure = Either LibauthSetupFailure AcceptToMemoryPoolResult

data LibauthSetupFailure = CanNotParseTx
  deriving (Show)

libauthRoot :: FilePath
libauthRoot = "." </> "test" </> "libauth"

findAndLoad :: FilePath -> Int -> Int -> IO [LibauthTest]
findAndLoad path expectedFileCount expectedTestCount = do
  files <- getTestPath path
  unless (length files == expectedFileCount) $
    error (printf "File count: %d" (length files))
  tests <- concat <$> mapM loadTests files
  unless (length tests == expectedTestCount) $
    error (printf "Test count: %d" (length tests))
  pure tests
  where
    getTestPath :: FilePath -> IO [FilePath]
    getTestPath component = do
      let dir = libauthRoot </> component
      files <- listDirectory dir
      let vmbTests = filter (T.isSuffixOf vmbSuffix) (T.pack <$> files)
       in pure $ (dir </>) . T.unpack . trimSuffix <$> vmbTests
      where
        trimSuffix :: Text -> Text
        trimSuffix str =
          fromMaybe (error $ "getTestPath " <> show str) $
            T.stripSuffix vmbSuffix str
        vmbSuffix = ".vmb_tests.json"

loadTests :: FilePath -> IO [LibauthTest]
loadTests path = do
  let testFile = path <.> "vmb_tests" <.> "json"
      testClass = T.pack $ takeDirectory path
      standardLimitsFile = path <.> "standard_limits" <.> "json"
      nonstandardLimitsFile = path <.> "nonstandard_limits" <.> "json"
      standardResultsFile = path <.> "standard_results" <.> "json"
      nonstandardResultsFile = path <.> "nonstandard_results" <.> "json"
  tests <- A.eitherDecodeFileStrict testFile
  sResults <- A.eitherDecodeFileStrict standardResultsFile
  nsResults <- A.eitherDecodeFileStrict nonstandardResultsFile
  sLimits <- A.eitherDecodeFileStrict standardLimitsFile
  nsLimits <- A.eitherDecodeFileStrict nonstandardLimitsFile
  let tests' =
        fmap
          <$> ( toLibauthTest
                  <$> Right testClass
                  <*> sResults
                  <*> nsResults
                  <*> sLimits
                  <*> nsLimits
              )
          <*> tests
  case tests' of
    Right t -> pure t
    Left _ -> error "loadTests"

toLibauthTest ::
  Text ->
  LibAuthFileResults ->
  LibAuthFileResults ->
  LibAuthFileLimits ->
  LibAuthFileLimits ->
  LibAuthFileVmbTestsRecord ->
  LibauthTest
toLibauthTest testClass srMap nsrMap slMap nslMap r =
  let shortId = r.shortId
      inputIndex = fromMaybe 0 r.inputIndex
      testTransactionHex = r.testTransactionHex
      sourceOutputsHex = r.sourceOutputsHex
      standardResult = convert $ fromMaybe err (M.lookup r.shortId srMap)
      nonstandardResult = convert $ fromMaybe err (M.lookup r.shortId nsrMap)
      standardLimits =
        let (densityControlLength, maxCost, cost, _) =
              fromMaybe err (M.lookup r.shortId slMap)
         in Limits {..}
      nonstandardLimits =
        let (densityControlLength, maxCost, cost, _) =
              fromMaybe err (M.lookup r.shortId nslMap)
         in Limits {..}
   in LibauthTest {..}
  where
    err = error "toLibauthTest"

    convert :: A.Value -> ExpectedResult
    convert (A.Bool True) = Success
    convert (A.String errStr) = Err errStr
    convert _ = error "toLibauthTest / convert"

printSummary :: [a] -> [b] -> IO ()
printSummary selectedTests allTests =
  if length selectedTests /= length allTests
    then
      printf "Running %d of %d tests\n" (length selectedTests) (length allTests)
    else
      printf "Running all %d tests\n" (length selectedTests)

runTest :: TestMode -> LibauthTest -> IO ResultOrSetupFailure
runTest testMode test@(LibauthTest {..}) = do
  let (verifyScript, vmParams, mode) = testModeToVmSetup testMode
  case testToTxAndUtxos test of
    Right (tx, txOuts) -> do
      let txContext = fromJust $ mkTxContext tx inputIndex txOuts.get
          validationResult =
            acceptToMemoryPool verifyScript txContext vmParams mode
      -- debugOutput tx txOuts vmParams
      pure $ Right validationResult
    Left err -> pure $ Left err
  where
    debugOutput :: Tx -> TxOuts -> VmParams -> IO ()
    debugOutput tx txOuts vmParams = do
      printf "*test*\n"
      pPrintLightBg test

      printf "\n*txOuts / locking scripts*\n"
      pPrintLightBg txOuts
      pPrintLightBg $ codeL1ToCodeL2 . (.scriptPubKey) <$> txOuts.get
      print $ solver vmParams . (.scriptPubKey) <$> txOuts.get

      printf "\n*txIns / unlocking scripts*\n"
      pPrintLightBg $ encodeHex . (.scriptSig) <$> tx.inputs
      pPrintLightBg $ codeL1ToCodeL2 . (.scriptSig) <$> tx.inputs

      printf "\n*tx*\n"
      pPrintLightBg tx

testModeToVmSetup :: TestMode -> (VerifyScriptFun, VmParams, Mode)
testModeToVmSetup Standard2025 =
  (Vm2025.verifyScript, Vm2025.vmParamsStandard, Standard)
testModeToVmSetup Nonstandard2025 =
  (Vm2025.verifyScript, Vm2025.vmParamsNonStandard, Nonstandard)
testModeToVmSetup Standard2026 =
  (Vm2026.verifyScript, Vm2026.vmParamsStandard, Standard)
testModeToVmSetup Nonstandard2026 =
  (Vm2026.verifyScript, Vm2026.vmParamsNonStandard, Nonstandard)
testModeToVmSetup StandardSpec =
  (VmSpec.verifyScript, VmSpec.vmParamsStandard, Standard)
testModeToVmSetup NonstandardSpec =
  (VmSpec.verifyScript, VmSpec.vmParamsNonStandard, Nonstandard)

testToTxAndUtxos :: LibauthTest -> Either LibauthSetupFailure (Tx, TxOuts)
testToTxAndUtxos LibauthTest {..} = do
  tx <-
    either
      (\(_, _, _) -> Left CanNotParseTx)
      (\(_, _, res) -> Right res)
      (decodeOrFail (B.fromStrict $ fromJust $ decodeHex testTransactionHex))
  txOuts <-
    either
      (\(_, _, _) -> Left CanNotParseTx)
      (\(_, _, res) -> Right res)
      (decodeOrFail (B.fromStrict $ fromJust $ decodeHex sourceOutputsHex))
  pure (tx, txOuts)

verifyResult :: TestMode -> LibauthTest -> ResultOrSetupFailure -> IO ()
verifyResult testMode test resultOrFailure =
  let expected =
        if inStandardMode testMode
          then test.standardResult
          else test.nonstandardResult
   in case expected of
        Success -> verifyTxApproved testMode test resultOrFailure
        Err _ -> verifyTxNotApproved testMode test resultOrFailure

inStandardMode :: TestMode -> Bool
inStandardMode Standard2025 = True
inStandardMode Nonstandard2025 = False
inStandardMode Standard2026 = True
inStandardMode Nonstandard2026 = False
inStandardMode StandardSpec = True
inStandardMode NonstandardSpec = False

verifyTxApproved :: TestMode -> LibauthTest -> ResultOrSetupFailure -> IO ()
verifyTxApproved testMode test res =
  case res of
    Right testResult ->
      case testResult of
        (Left err) -> do
          assertFailure
            (printf "%s: validation failure %s" test.shortId (show err))
        (Right res'@(Left (err, _))) -> do
          let displayOpts = defaultDisplayOpts {showMetrics = True}
          dumpVerifyScriptResult displayOpts res'
          assertFailure (printf "%s: failed with %s" test.shortId (show err))
        (Right (Right _res')) -> do
          -- dumpVerifyScriptResult Nothing True res'
          assertBool "metrics" (verifyMetrics testMode test testResult)
          pure ()
    Left err ->
      assertFailure (printf "%s: failed with %s" test.shortId (show err))

verifyTxNotApproved :: TestMode -> LibauthTest -> ResultOrSetupFailure -> IO ()
verifyTxNotApproved testMode test res =
  case res of
    Right testResult ->
      case testResult of
        (Left _err) -> pure ()
        (Right (Left (err, r))) -> do
          unless
            ( ( isNothing r.scriptSigResult
                  && isNothing r.scriptPubKeyResult
                  && isNothing r.scriptRedeemResult
              )
                || T.isInfixOf "invalid" test.testClass
                || standardModeIntraOpError err
            )
            $ assertBool
              ("metrics check after " <> show err)
              (verifyMetrics testMode test testResult)
        (Right _res'@(Right _)) -> do
          -- let displayOpts = defaultDisplayOpts {showMetrics = True}
          -- dumpVerifyScriptResult displayOpts res'
          assertFailure (printf "%s: passed validation" test.shortId)
    Left _ -> pure ()

-- Op evaluation errors specific to standard mode. We don't check metrics for
-- in-the-middle-of-op-evaluation errors, since we do accounting in such cases
-- differently from Libauth.
standardModeIntraOpError :: ScriptError -> Bool
standardModeIntraOpError SeDiscourageUpgradableNops = True
standardModeIntraOpError _ = False

verifyMetrics :: TestMode -> LibauthTest -> AcceptToMemoryPoolResult -> Bool
verifyMetrics testMode test validationResult =
  let limits =
        if inStandardMode testMode
          then test.standardLimits
          else test.nonstandardLimits
      VmState {metrics = VmMetrics {cost}} = case validationResult of
        Right (Right vsResult) -> getVmState vsResult
        Right (Left (_, vsResult)) -> getVmState vsResult
        _ -> canNotHappen
      res = cost == limits.cost
   in if res
        then res
        else traceShow (test.shortId, test.testClass, cost, limits.cost) res
  where
    getVmState VerifyScriptResult {..} =
      fromMaybe
        (fromMaybe (fromMaybe canNotHappen scriptSigResult) scriptPubKeyResult)
        scriptRedeemResult

tryTest :: TestMode -> LibauthTest -> IO ()
tryTest testMode test@(LibauthTest {shortId}) = do
  res <- runTest testMode test
  case res of
    Right testResult ->
      case testResult of
        (Left err) -> do
          printf " , \"%s\" -- validation failure %s\n" shortId (show err)
        (Right (Left (err, _))) -> do
          printf " , \"%s\" -- failed with %s\n" shortId (show err)
        (Right (Right _res')) -> do
          printf " , \"%s\" -- " shortId
          if verifyMetrics testMode test testResult
            then printf "passed validation.\n"
            else printf "passed validation but failed metrics!\n"
    Left err ->
      printf " , \"%s\" -- failure %s\n" shortId (show err)
  1 @?= (1 :: Int)
