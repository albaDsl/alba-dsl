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
    tryTest,
    verifyTxApproved,
    verifyTxNotApproved,
  )
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestLibauthVectorsExclusions2026
  ( excludeInvalid,
    excludeNonStandardInStandardMode,
    excludeStandard,
  )
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

testLibauthVectors2026 :: TestTree
testLibauthVectors2026 =
  testGroup
    "Libauth vectors"
    [ testCase "bch_2026_standard in standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` excludeStandard) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript standard >=> verifyTxApproved) tests',
      testCase "bch_2026_standard in non-standard-mode" $ do
        tests <- standardTests
        let tests' = filterTests (`notElem` excludeStandard) tests
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
                (`notElem` excludeNonStandardInStandardMode)
                tests
        printSummary tests' tests
        mapM_ (runTest verifyScript standard >=> verifyTxNotApproved) tests',
      testCase "bch_2026_invalid in standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` excludeInvalid) tests
        printSummary tests' tests
        mapM_ (runTest verifyScript standard >=> verifyTxNotApproved) tests',
      testCase "bch_2026_invalid in non-standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` excludeInvalid) tests
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
    <$> [ "chip.eval.vmb_tests",
          "chip.flow-control.vmb_tests",
          "chip.loops.vmb_tests",
          "chip.p2s.vmb_tests",
          "core.benchmarks.arithmetic.add-sub.vmb_tests",
          "core.benchmarks.arithmetic.div-mod.vmb_tests",
          "core.benchmarks.arithmetic.mul.vmb_tests",
          "core.benchmarks.baseline.vmb_tests",
          "core.benchmarks.bitwise.vmb_tests",
          "core.benchmarks.hashing.vmb_tests",
          "core.benchmarks.roll.vmb_tests",
          "core.benchmarks.signature-checking.bms-ecdsa.vmb_tests",
          "core.benchmarks.signature-checking.bms-schnorr.vmb_tests",
          "core.benchmarks.signature-checking.p2pk.vmb_tests",
          "core.benchmarks.signature-checking.p2pkh.vmb_tests",
          "core.benchmarks.stack.vmb_tests",
          "core.bigint-basics.vmb_tests",
          "core.bigint-limits.binary.vmb_tests",
          "core.bigint-limits.ternary.vmb_tests",
          "core.bigint-limits.unary.vmb_tests",
          "core.bigint.0notequal.vmb_tests",
          "core.bigint.1add.vmb_tests",
          "core.bigint.1sub.vmb_tests",
          "core.bigint.abs.vmb_tests",
          "core.bigint.add.vmb_tests",
          "core.bigint.bin2num.vmb_tests",
          "core.bigint.booland.vmb_tests",
          "core.bigint.boolor.vmb_tests",
          "core.bigint.div.vmb_tests",
          "core.bigint.greaterthan.vmb_tests",
          "core.bigint.greaterthanorequal.vmb_tests",
          "core.bigint.lessthan.vmb_tests",
          "core.bigint.lessthanorequal.vmb_tests",
          "core.bigint.max.vmb_tests",
          "core.bigint.min.vmb_tests",
          "core.bigint.mod.vmb_tests",
          "core.bigint.mul.vmb_tests",
          "core.bigint.negate.vmb_tests",
          "core.bigint.not.vmb_tests",
          "core.bigint.num2bin.vmb_tests",
          "core.bigint.numequal.vmb_tests",
          "core.bigint.numequalverify.vmb_tests",
          "core.bigint.numnotequal.vmb_tests",
          "core.bigint.sub.vmb_tests",
          "core.bigint.within.vmb_tests",
          "core.cashtokens.vmb_tests",
          "core.conditionals.vmb_tests",
          "core.copy.vmb_tests",
          "core.data-signatures.vmb_tests",
          "core.disabled.vmb_tests",
          "core.formatting.vmb_tests",
          "core.hashing.vmb_tests",
          "core.inspection.vmb_tests",
          "core.limits.vmb_tests",
          "core.nop.vmb_tests",
          "core.push.bytes.vmb_tests",
          "core.push.data.limits.vmb_tests",
          "core.push.data.vmb_tests",
          "core.push.minimal.vmb_tests",
          "core.push.numbers.vmb_tests",
          "core.push.ops.vmb_tests",
          "core.signature-checking.multisig.m-of-15.vmb_tests",
          "core.signature-checking.multisig.m-of-20.vmb_tests",
          "core.signature-checking.multisig.m-of-3.vmb_tests",
          "core.signature-checking.multisig.signing-serialization.vmb_tests",
          "core.signing-serialization.vmb_tests"
        ]

bch2026NonStandardFiles :: [String]
bch2026NonStandardFiles =
  (\x -> "bch_2026_nonstandard" </> x <.> "json")
    <$> [ "chip.eval.vmb_tests",
          "core.benchmarks.arithmetic.add-sub.vmb_tests",
          "core.benchmarks.arithmetic.div-mod.vmb_tests",
          "core.benchmarks.arithmetic.mul.vmb_tests",
          "core.benchmarks.hashing-bytes.packed.vmb_tests",
          "core.benchmarks.hashing-iters.packed.vmb_tests",
          "core.benchmarks.hashing.vmb_tests",
          "core.benchmarks.stack.vmb_tests",
          "core.bigint.0notequal.vmb_tests",
          "core.bigint.1add.vmb_tests",
          "core.bigint.1sub.vmb_tests",
          "core.bigint.abs.vmb_tests",
          "core.bigint.add.vmb_tests",
          "core.bigint.bin2num.vmb_tests",
          "core.bigint.booland.vmb_tests",
          "core.bigint.boolor.vmb_tests",
          "core.bigint.div.vmb_tests",
          "core.bigint.greaterthan.vmb_tests",
          "core.bigint.greaterthanorequal.vmb_tests",
          "core.bigint.lessthan.vmb_tests",
          "core.bigint.lessthanorequal.vmb_tests",
          "core.bigint.max.vmb_tests",
          "core.bigint.min.vmb_tests",
          "core.bigint.mod.vmb_tests",
          "core.bigint.mul.vmb_tests",
          "core.bigint.negate.vmb_tests",
          "core.bigint.not.vmb_tests",
          "core.bigint.num2bin.vmb_tests",
          "core.bigint.numequal.vmb_tests",
          "core.bigint.numequalverify.vmb_tests",
          "core.bigint.numnotequal.vmb_tests",
          "core.bigint.sub.vmb_tests",
          "core.bigint.within.vmb_tests",
          "core.cashtokens.vmb_tests",
          "core.conditionals.vmb_tests",
          "core.inspection.vmb_tests",
          "core.limits.vmb_tests",
          "core.nop.vmb_tests",
          "core.push.data.vmb_tests",
          "core.signature-checking.multisig.m-of-15.vmb_tests",
          "core.signature-checking.multisig.m-of-20.vmb_tests"
        ]

bch2026InvalidFiles :: [String]
bch2026InvalidFiles =
  (\x -> "bch_2026_invalid" </> x <.> "json")
    <$> [ "chip.eval.vmb_tests",
          "chip.flow-control.vmb_tests",
          "chip.loops.vmb_tests",
          "core.benchmarks.bitwise.vmb_tests",
          "core.benchmarks.hashing.vmb_tests",
          "core.benchmarks.roll.vmb_tests",
          "core.benchmarks.stack.vmb_tests",
          "core.bigint-basics.vmb_tests",
          "core.bigint-limits.binary.vmb_tests",
          "core.bigint-limits.ternary.vmb_tests",
          "core.bigint-limits.unary.vmb_tests",
          "core.bigint.1add.vmb_tests",
          "core.bigint.1sub.vmb_tests",
          "core.bigint.abs.vmb_tests",
          "core.bigint.add.vmb_tests",
          "core.bigint.bin2num.vmb_tests",
          "core.bigint.booland.vmb_tests",
          "core.bigint.boolor.vmb_tests",
          "core.bigint.div.vmb_tests",
          "core.bigint.greaterthan.vmb_tests",
          "core.bigint.greaterthanorequal.vmb_tests",
          "core.bigint.lessthan.vmb_tests",
          "core.bigint.lessthanorequal.vmb_tests",
          "core.bigint.max.vmb_tests",
          "core.bigint.min.vmb_tests",
          "core.bigint.mod.vmb_tests",
          "core.bigint.mul.vmb_tests",
          "core.bigint.negate.vmb_tests",
          "core.bigint.num2bin.vmb_tests",
          "core.bigint.numequal.vmb_tests",
          "core.bigint.numequalverify.vmb_tests",
          "core.bigint.numnotequal.vmb_tests",
          "core.bigint.sub.vmb_tests",
          "core.bigint.within.vmb_tests",
          "core.cashtokens.vmb_tests",
          "core.conditionals.vmb_tests",
          "core.disabled.vmb_tests",
          "core.inspection.vmb_tests",
          "core.limits.vmb_tests",
          "core.nop.vmb_tests",
          "core.push-only.vmb_tests",
          "core.push.bytes.vmb_tests",
          "core.push.data.limits.vmb_tests",
          "core.push.data.vmb_tests",
          "core.push.minimal.vmb_tests",
          "core.push.numbers.vmb_tests",
          "core.push.ops.vmb_tests",
          "core.signature-checking.multisig.m-of-15.vmb_tests",
          "core.signature-checking.multisig.m-of-20.vmb_tests",
          "core.signature-checking.multisig.m-of-3.vmb_tests",
          "core.signing-serialization.vmb_tests"
        ]
