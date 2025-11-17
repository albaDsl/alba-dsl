-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestLibauthVectors2025 (testLibauthVectors2025) where

import Alba.Misc.Utils (decodeHex)
import Alba.Node.Validation (Mode (..))
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxOuts (..))
import Alba.Vm.Bch2025
  ( LogDisplayOpts (..),
    VmParams,
    defaultDisplayOpts,
    mkTxContext,
    startState,
    verifyScript,
    vmParamsNonStandard,
    vmParamsStandard,
  )
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
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
  ( LibAuthLimits,
    LibAuthTest (..),
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
import TestLibauthVectorsExclusions2025
  ( excludeInvalid,
    excludeNonStandardInStandardMode,
    excludeStandard,
  )
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

testLibauthVectors2025 :: TestTree
testLibauthVectors2025 =
  testGroup
    "Libauth vectors 2025"
    [ testCase "bch_2025_standard in standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` excludeStandard) tests
        -- tests' = filterTests (== "wjg8aj") tests
        printSummary tests' tests
        mapM_ (runTest Standard2025 >=> verifyTxApproved) tests',
      testCase "bch_2025_standard in non-standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` excludeStandard) tests
        printSummary tests' tests
        mapM_ (runTest Nonstandard2025 >=> verifyTxApproved) tests',
      testCase "bch_2025_nonstandard in standard-mode" $ do
        tests <- nonStandardTests
        let tests' =
              filterTests
                (`notElem` excludeNonStandardInStandardMode)
                tests
        printSummary tests' tests
        mapM_ (runTest Standard2025 >=> verifyTxNotApproved) tests',
      testCase "bch_2025_nonstandard in non-standard-mode" $ do
        tests <- nonStandardTests
        let tests' = filterTests (const True) tests
        printSummary tests' tests
        mapM_ (runTest Nonstandard2025 >=> verifyTxApproved) tests',
      testCase "bch_2025_invalid in standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` excludeInvalid) tests
        printSummary tests' tests
        mapM_ (runTest Standard2025 >=> verifyTxNotApproved) tests',
      testCase "bch_2025_invalid in non-standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` excludeInvalid) tests
        printSummary tests' tests
        mapM_ (runTest Nonstandard2025 >=> verifyTxNotApproved) tests'
    ]
  where
    standardTests = concat <$> mapM loadTests bch2025StandardFiles

    nonStandardTests = concat <$> mapM loadTests bch2025NonStandardFiles

    invalidTests = concat <$> mapM loadTests bch2025InvalidFiles

    filterTests check =
      filter
        (\LibAuthTest {test = LibAuthTestRecord {shortId}} -> check shortId)

    standard = vmParamsStandard

    nonStandard = vmParamsNonStandard

bch2025StandardFiles :: [String]
bch2025StandardFiles =
  ("bch_2025_standard" </>)
    <$> [ "core.benchmarks.arithmetic.add-sub",
          "core.benchmarks.arithmetic.div-mod",
          "core.benchmarks.arithmetic.mul",
          "core.benchmarks.baseline",
          "core.benchmarks.bitwise",
          "core.benchmarks.hashing",
          "core.benchmarks.roll",
          "core.benchmarks.signature-checking.bms-ecdsa",
          "core.benchmarks.signature-checking.bms-schnorr",
          "core.benchmarks.signature-checking.p2pk",
          "core.benchmarks.signature-checking.p2pkh",
          "core.benchmarks.stack",
          "core.bigint-basics",
          "core.bigint-limits.binary",
          "core.bigint-limits.ternary",
          "core.bigint-limits.unary",
          "core.bigint.0notequal",
          "core.bigint.1add",
          "core.bigint.1sub",
          "core.bigint.abs",
          "core.bigint.add",
          "core.bigint.bin2num",
          "core.bigint.booland",
          "core.bigint.boolor",
          "core.bigint.div",
          "core.bigint.greaterthan",
          "core.bigint.greaterthanorequal",
          "core.bigint.lessthan",
          "core.bigint.lessthanorequal",
          "core.bigint.max",
          "core.bigint.min",
          "core.bigint.mod",
          "core.bigint.mul",
          "core.bigint.negate",
          "core.bigint.not",
          "core.bigint.num2bin",
          "core.bigint.numequal",
          "core.bigint.numequalverify",
          "core.bigint.numnotequal",
          "core.bigint.sub",
          "core.bigint.within",
          "core.cashtokens",
          "core.conditionals",
          "core.copy",
          "core.data-signatures",
          "core.disabled",
          "core.formatting",
          "core.hashing",
          "core.inspection",
          "core.limits",
          "core.nop",
          "core.push.bytes",
          "core.push.data.limits",
          "core.push.data",
          "core.push.minimal",
          "core.push.numbers",
          "core.push.ops",
          "core.signature-checking.multisig.m-of-15",
          "core.signature-checking.multisig.m-of-20",
          "core.signature-checking.multisig.m-of-3",
          "core.signature-checking.multisig.signing-serialization",
          "core.signing-serialization"
        ]

bch2025NonStandardFiles :: [String]
bch2025NonStandardFiles =
  ("bch_2025_nonstandard" </>)
    <$> [ "core.benchmarks.arithmetic.add-sub",
          "core.benchmarks.arithmetic.div-mod",
          "core.benchmarks.arithmetic.mul",
          "core.benchmarks.hashing-bytes.packed",
          "core.benchmarks.hashing-iters.packed",
          "core.benchmarks.hashing",
          "core.benchmarks.stack",
          "core.bigint-basics",
          "core.bigint-limits.binary",
          "core.bigint-limits.ternary",
          "core.bigint-limits.unary",
          "core.bigint.0notequal",
          "core.bigint.1add",
          "core.bigint.1sub",
          "core.bigint.abs",
          "core.bigint.add",
          "core.bigint.bin2num",
          "core.bigint.booland",
          "core.bigint.boolor",
          "core.bigint.div",
          "core.bigint.greaterthan",
          "core.bigint.greaterthanorequal",
          "core.bigint.lessthan",
          "core.bigint.lessthanorequal",
          "core.bigint.max",
          "core.bigint.min",
          "core.bigint.mod",
          "core.bigint.mul",
          "core.bigint.negate",
          "core.bigint.not",
          "core.bigint.num2bin",
          "core.bigint.numequal",
          "core.bigint.numequalverify",
          "core.bigint.numnotequal",
          "core.bigint.sub",
          "core.bigint.within",
          "core.cashtokens",
          "core.conditionals",
          "core.copy",
          "core.data-signatures",
          "core.disabled",
          "core.formatting",
          "core.hashing",
          "core.inspection",
          "core.limits",
          "core.nop",
          "core.push.bytes",
          "core.push.data.limits",
          "core.push.data",
          "core.push.minimal",
          "core.push.numbers",
          "core.push.ops",
          "core.signature-checking.multisig.m-of-15",
          "core.signature-checking.multisig.m-of-20",
          "core.signing-serialization"
        ]

bch2025InvalidFiles :: [String]
bch2025InvalidFiles =
  ("bch_2025_invalid" </>)
    <$> [ "core.benchmarks.bitwise",
          "core.benchmarks.hashing",
          "core.benchmarks.roll",
          "core.benchmarks.stack",
          "core.bigint-basics",
          "core.bigint-limits.binary",
          "core.bigint-limits.ternary",
          "core.bigint-limits.unary",
          "core.bigint.1add",
          "core.bigint.1sub",
          "core.bigint.abs",
          "core.bigint.add",
          "core.bigint.bin2num",
          "core.bigint.booland",
          "core.bigint.boolor",
          "core.bigint.div",
          "core.bigint.greaterthan",
          "core.bigint.greaterthanorequal",
          "core.bigint.lessthan",
          "core.bigint.lessthanorequal",
          "core.bigint.max",
          "core.bigint.min",
          "core.bigint.mod",
          "core.bigint.mul",
          "core.bigint.negate",
          "core.bigint.num2bin",
          "core.bigint.numequal",
          "core.bigint.numequalverify",
          "core.bigint.numnotequal",
          "core.bigint.sub",
          "core.bigint.within",
          "core.cashtokens",
          "core.conditionals",
          "core.disabled",
          "core.inspection",
          "core.limits",
          "core.nop",
          "core.push-only",
          "core.push.bytes",
          "core.push.data.limits",
          "core.push.data",
          "core.push.minimal",
          "core.push.numbers",
          "core.push.ops",
          "core.signature-checking.multisig.m-of-15",
          "core.signature-checking.multisig.m-of-20",
          "core.signature-checking.multisig.m-of-3",
          "core.signing-serialization"
        ]
