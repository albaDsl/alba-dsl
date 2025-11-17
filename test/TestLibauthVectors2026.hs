-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestLibauthVectors2026 (testLibauthVectors2026) where

import Alba.Misc.Utils (decodeHex)
import Alba.Node.Validation (Mode (..), acceptToMemoryPool)
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
import Alba.Vm.Common.LoggingText (dumpVerifyScriptResult)
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
  ( LibAuthTest (..),
    LibAuthTestRecord (..),
    TestMode (..),
    loadTests,
    printSummary,
    runTest,
    tryTest,
    verifyTxApproved,
    verifyTxNotApproved,
  )
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestLibauthVectorsExclusions2026
  ( excludeInvalid,
    excludeNonStandardInNonStandardMode,
    excludeNonStandardInStandardMode,
    excludeStandard,
  )
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

testLibauthVectors2026 :: TestTree
testLibauthVectors2026 =
  testGroup
    "Libauth vectors 2026"
    [ testCase "bch_2026_standard in standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` excludeStandard) tests
            -- let tests' = filterTests (== "dyxfml") tests
            mode = Standard2026
        printSummary tests' tests
        mapM_ (runTest mode >=> verifyTxApproved mode) tests',
      testCase "bch_2026_standard in non-standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` excludeStandard) tests
            mode = Nonstandard2026
        printSummary tests' tests
        mapM_ (runTest mode >=> verifyTxApproved mode) tests',
      testCase "bch_2026_nonstandard in standard-mode" $ do
        tests <- nonStandardTests
        let tests' =
              filterTests (`notElem` excludeNonStandardInStandardMode) tests
            mode = Standard2026
        printSummary tests' tests
        mapM_ (runTest mode >=> verifyTxNotApproved mode) tests',
      testCase "bch_2026_nonstandard in non-standard-mode" $ do
        tests <- nonStandardTests
        let tests' =
              filterTests (`notElem` excludeNonStandardInNonStandardMode) tests
            mode = Nonstandard2026
        printSummary tests' tests
        mapM_ (runTest mode >=> verifyTxApproved mode) tests',
      testCase "bch_2026_invalid in standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` excludeInvalid) tests
            mode = Standard2026
        printSummary tests' tests
        mapM_ (runTest mode >=> verifyTxNotApproved mode) tests',
      testCase "bch_2026_invalid in non-standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` excludeInvalid) tests
            mode = Nonstandard2026
        printSummary tests' tests
        mapM_ (runTest mode >=> verifyTxNotApproved mode) tests'
    ]
  where
    standardTests = concat <$> mapM loadTests bch2026StandardFiles

    nonStandardTests = concat <$> mapM loadTests bch2026NonStandardFiles

    invalidTests = concat <$> mapM loadTests bch2026InvalidFiles

    filterTests check = filter (\test -> check test.testRecord.shortId)

    standard = vmParamsStandard

    nonStandard = vmParamsNonStandard

bch2026StandardFiles :: [String]
bch2026StandardFiles =
  ("bch_2026_standard" </>)
    <$> [ "core.bigint-basics",
          "core.copy",
          "core.cashtokens",
          "core.bigint-limits.ternary",
          "core.benchmarks.arithmetic.div-mod",
          "core.bigint.booland",
          "core.benchmarks.baseline",
          "core.bigint.numnotequal",
          "core.push.data",
          "core.benchmarks.signature-checking.bms-schnorr",
          "core.bigint-limits.unary",
          "core.benchmarks.bitwise",
          "core.bigint.lessthanorequal",
          "core.bigint.max",
          "core.signature-checking.multisig.m-of-3",
          "core.push.data.limits",
          "core.bigint.negate",
          "core.bigint.abs",
          "core.benchmarks.arithmetic.add-sub",
          "core.nop",
          "core.signature-checking.multisig.pubkey-validation",
          "core.push.numbers",
          "chip.functions",
          "core.disabled",
          "core.push.ops",
          "core.bigint.greaterthan",
          "core.signature-checking.multisig.m-of-20",
          "core.bigint.greaterthanorequal",
          "chip.p2s",
          "chip.benchmarks.bitwise",
          "core.bigint.boolor",
          "core.push.minimal",
          "core.bigint.1sub",
          "core.limits",
          "chip.flow-control",
          "core.inspection",
          "core.bigint.min",
          "core.benchmarks.hashing",
          "chip.bitwise",
          "core.signature-checking.multisig.signing-serialization",
          "core.bigint.1add",
          "core.bigint.numequal",
          "core.conditionals",
          "core.signature-checking.multisig.m-of-15",
          "core.bigint.lessthan",
          "core.bigint.div",
          "core.bigint.within",
          "core.bigint.0notequal",
          "core.bigint.mod",
          "core.bigint.mul",
          "core.bigint.num2bin",
          "core.data-signatures",
          "core.bigint-limits.binary",
          "core.benchmarks.signature-checking.bms-ecdsa",
          "core.bigint.sub",
          "chip.loops",
          "core.bigint.not",
          "core.formatting",
          "core.benchmarks.arithmetic.mul",
          "core.benchmarks.roll",
          "core.benchmarks.stack",
          "core.bigint.add",
          "core.benchmarks.signature-checking.p2pkh",
          "core.hashing",
          "core.benchmarks.signature-checking.p2pk",
          "core.push.bytes",
          "core.bigint.bin2num",
          "core.signing-serialization",
          "core.bigint.numequalverify"
        ]

bch2026NonStandardFiles :: [String]
bch2026NonStandardFiles =
  ("bch_2026_nonstandard" </>)
    <$> [ "core.cashtokens",
          "core.benchmarks.arithmetic.div-mod",
          "core.benchmarks.hashing-iters.packed",
          "core.bigint.booland",
          "core.bigint.numnotequal",
          "core.push.data",
          "core.bigint.lessthanorequal",
          "core.bigint.max",
          "core.bigint.negate",
          "core.bigint.abs",
          "core.benchmarks.arithmetic.add-sub",
          "core.benchmarks.hashing-bytes.packed",
          "core.nop",
          "core.bigint.greaterthan",
          "core.signature-checking.multisig.m-of-20",
          "core.bigint.greaterthanorequal",
          "core.bigint.boolor",
          "core.bigint.1sub",
          "core.limits",
          "core.inspection",
          "core.bigint.min",
          "core.benchmarks.hashing",
          "chip.bitwise",
          "core.bigint.1add",
          "core.bigint.numequal",
          "core.conditionals",
          "core.signature-checking.multisig.m-of-15",
          "core.bigint.lessthan",
          "core.bigint.div",
          "core.bigint.within",
          "core.bigint.0notequal",
          "core.bigint.mod",
          "core.bigint.mul",
          "core.bigint.num2bin",
          "core.bigint.sub",
          "core.bigint.not",
          "core.benchmarks.arithmetic.mul",
          "core.benchmarks.stack",
          "core.bigint.add",
          "core.bigint.bin2num",
          "core.bigint.numequalverify"
        ]

bch2026InvalidFiles :: [String]
bch2026InvalidFiles =
  ("bch_2026_invalid" </>)
    <$> [ "core.bigint-basics",
          "core.cashtokens",
          "core.bigint-limits.ternary",
          "core.bigint.booland",
          "core.bigint.numnotequal",
          "core.push.data",
          "core.bigint-limits.unary",
          "core.benchmarks.bitwise",
          "core.bigint.lessthanorequal",
          "core.bigint.max",
          "core.signature-checking.multisig.m-of-3",
          "core.push.data.limits",
          "core.bigint.negate",
          "core.bigint.abs",
          "core.nop",
          "core.signature-checking.multisig.pubkey-validation",
          "core.push.numbers",
          "chip.functions",
          "core.disabled",
          "core.push.ops",
          "core.bigint.greaterthan",
          "core.signature-checking.multisig.m-of-20",
          "core.bigint.greaterthanorequal",
          "chip.benchmarks.bitwise",
          "core.bigint.boolor",
          "core.push.minimal",
          "core.bigint.1sub",
          "core.limits",
          "chip.flow-control",
          "core.inspection",
          "core.bigint.min",
          "core.benchmarks.hashing",
          "chip.bitwise",
          "core.bigint.1add",
          "core.bigint.numequal",
          "core.conditionals",
          "core.signature-checking.multisig.m-of-15",
          "core.bigint.lessthan",
          "core.bigint.div",
          "core.bigint.within",
          "core.bigint.mod",
          "core.bigint.mul",
          "core.bigint.num2bin",
          "core.bigint-limits.binary",
          "core.bigint.sub",
          "chip.loops",
          "core.benchmarks.roll",
          "core.benchmarks.stack",
          "core.bigint.add",
          "core.push.bytes",
          "core.bigint.bin2num",
          "core.signing-serialization",
          "core.bigint.numequalverify",
          "core.push-only"
        ]
