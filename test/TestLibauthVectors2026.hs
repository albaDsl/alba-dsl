-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module TestLibauthVectors2026 (testLibauthVectors2026) where

import Alba.Misc.Utils (decodeHex)
import Alba.Node.Validation (acceptToMemoryPool)
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxOuts (..))
import Alba.Vm.Bch2026
  ( LogDisplayOpts (..),
    VmParams,
    defaultDisplayOpts,
    mkTxContext,
    startState,
    verifyScript,
    vmParamsNonStandard,
    vmParamsStandard,
  )
import Alba.Vm.Common
  ( ScriptError,
    VerifyScriptResult,
    dumpVerifyScriptResult,
  )
import Alba.Vm.Common.Logging (dumpVerifyScriptResult)
import Alba.Vm.Common.VmState (VerifyScriptResult)
import Control.Monad ((>=>))
import Data.Aeson qualified as A
import Data.Binary (decodeOrFail)
import Data.ByteString qualified as B
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import Data.Vector qualified as V
import LibauthSupport
  ( LibAuthTestRecord (..),
    loadTests,
    printSummary,
    runTest,
    verifyTxApproved,
    verifyTxNotApproved,
  )
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestLibauthVectorsExclusions2026
  ( exclude2026Invalid,
    exclude2026NonStandardInStandardMode,
    exclude2026Standard,
  )
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

testLibauthVectors2026 :: TestTree
testLibauthVectors2026 =
  testGroup
    "Libauth vectors"
    [ testCase "bch_2026_standard in standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` exclude2026Standard) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript standard >=> verifyTxApproved) tests',
      testCase "bch_2026_standard in non-standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` exclude2026Standard) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript nonStandard >=> verifyTxApproved) tests',
      testCase "bch_2026_nonstandard in non-standard-mode" $ do
        tests <- nonStandardTests
        let tests' = filterTests (const True) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript nonStandard >=> verifyTxApproved) tests',
      testCase "bch_2026_nonstandard in standard-mode" $ do
        tests <- nonStandardTests
        let tests' =
              filterTests
                (`notElem` exclude2026NonStandardInStandardMode)
                tests
        printSummary tests' tests
        mapM_ (runTest verifyScript standard >=> verifyTxNotApproved) tests',
      testCase "bch_2026_invalid in standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` exclude2026Invalid) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript standard >=> verifyTxNotApproved) tests',
      testCase "bch_2026_invalid in non-standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` exclude2026Invalid) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript nonStandard >=> verifyTxNotApproved) tests'
    ]
  where
    standardTests = concat <$> mapM loadTests bch2026StandardFiles

    nonStandardTests = concat <$> mapM loadTests bch2026NonStandardFiles

    invalidTests = concat <$> mapM loadTests bch2026InvalidFiles

    filterTests check = filter (\LibAuthTestRecord {..} -> check shortId)

    standard = vmParamsStandard

    nonStandard = vmParamsNonStandard

bch2026StandardFiles :: [String]
bch2026StandardFiles =
  (\x -> "bch_2026_standard" </> x <.> "json")
    <$> ["chip.eval.vmb_tests"]

bch2026NonStandardFiles :: [String]
bch2026NonStandardFiles =
  (\x -> "bch_2026_nonstandard" </> x <.> "json")
    <$> ["chip.eval.vmb_tests"]

bch2026InvalidFiles :: [String]
bch2026InvalidFiles = (\x -> "bch_2026_invalid" </> x <.> "json") <$> []
