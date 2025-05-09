-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module TestLibauthVectors (testLibauthVectors) where

import Alba.Misc.Utils (decodeHex)
import Alba.Node.Validation (acceptToMemoryPool)
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxOuts (..))
import Alba.Vm.Bch2025
  ( LogDisplayOpts (..),
    VmParams,
    defaultDisplayOpts,
    evaluateScript,
    mkTxContext,
    startState,
    vmParamsNonStandard,
    vmParamsStandard,
  )
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
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
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestLibauthVectorsExclusions
  ( exclude2025Invalid,
    exclude2025NonStandardInStandardMode,
    exclude2025Standard,
  )
import Text.Pretty.Simple (pPrintLightBg)
import Text.Printf (printf)

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

data TestLibauthFailure = CanNotParseTx
  deriving (Show)

type Result =
  Either
    TestLibauthFailure
    ( Either
        ValidationFailure
        (Either (ScriptError, VerifyScriptResult) VerifyScriptResult)
    )

testLibauthVectors :: TestTree
testLibauthVectors =
  testGroup
    "Libauth vectors"
    [ testCase "bch_2025_standard in standard-mode" $ do
        tests <- standardTests
        printSummary tests tests
        mapM_ (runTest standard >=> verifyTxApproved) tests,
      testCase "bch_2025_standard in non-standard-mode" $ do
        tests <- standardTests
        printSummary tests tests
        mapM_ (runTest nonStandard >=> verifyTxApproved) tests,
      testCase "bch_2025_nonstandard in non-standard-mode" $ do
        tests <- nonStandardTests
        let tests' = filterTests (const True) tests
        printSummary tests' tests
        mapM_ (runTest nonStandard >=> verifyTxApproved) tests',
      testCase "bch_2025_nonstandard in standard-mode" $ do
        tests <- nonStandardTests
        let tests' =
              filterTests
                (`notElem` exclude2025NonStandardInStandardMode)
                tests
        printSummary tests' tests
        mapM_ (runTest standard >=> verifyTxNotApproved) tests',
      testCase "bch_2025_invalid in standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` exclude2025Invalid) tests
        printSummary tests' tests
        mapM_ (runTest standard >=> verifyTxNotApproved) tests',
      testCase "bch_2025_invalid in non-standard-mode" $ do
        tests <- invalidTests
        let tests' = filterTests (`notElem` exclude2025Invalid) tests
        printSummary tests' tests
        mapM_ (runTest nonStandard >=> verifyTxNotApproved) tests'
    ]
  where
    standardTests = concat <$> mapM loadTests bch2025StandardFiles

    nonStandardTests = concat <$> mapM loadTests bch2025NonStandardFiles

    invalidTests = concat <$> mapM loadTests bch2025InvalidFiles

    filterTests check = filter (\LibAuthTestRecord {..} -> check shortId)

    standard = vmParamsStandard

    nonStandard = vmParamsNonStandard

    loadTests :: String -> IO [LibAuthTestRecord]
    loadTests file = do
      res <- A.eitherDecodeFileStrict ("./test/libauth/" <> file)
      case res of
        Right res' -> pure res'
        Left err -> error err

    printSummary :: [a] -> [a] -> IO ()
    printSummary selectedTests allTests =
      if length selectedTests /= length allTests
        then
          printf
            "Running %d of %d tests\n"
            (length selectedTests)
            (length allTests)
        else
          printf "Running all %d tests\n" (length selectedTests)

    runTest :: VmParams -> LibAuthTestRecord -> IO (T.Text, Result)
    runTest vmParams test = do
      case txAndUtxos of
        Right (tx, txOuts) -> do
          let inputIndex = fromMaybe 0 test.inputIndex
              txContext = fromJust $ mkTxContext tx inputIndex txOuts.get
              res = acceptToMemoryPool txContext vmParams
          -- pPrintLightBg txOuts
          -- pPrintLightBg tx
          pure (test.shortId, Right res)
        Left err -> pure (test.shortId, Left err)
      where
        txAndUtxos :: Either TestLibauthFailure (Tx, TxOuts)
        txAndUtxos = do
          tx <-
            either
              (\(_, _, _) -> Left CanNotParseTx)
              (\(_, _, res) -> Right res)
              ( decodeOrFail
                  (B.fromStrict (fromJust $ decodeHex test.testTransactionHex))
              )
          txOuts <-
            either
              (\(_, _, _) -> Left CanNotParseTx)
              (\(_, _, res) -> Right res)
              ( decodeOrFail
                  (B.fromStrict $ fromJust $ decodeHex test.sourceOutputsHex)
              )
          pure (tx, txOuts)

    verifyTxApproved :: (T.Text, Result) -> IO ()
    verifyTxApproved (testId, res) =
      case res of
        Right (Left err) -> do
          assertFailure (printf "%s: validation failure %s" testId (show err))
        Right (Right res'@(Left (err, _))) -> do
          let displayOpts = defaultDisplayOpts {showMetrics = True}
          dumpVerifyScriptResult displayOpts res'
          assertFailure (printf "%s: failed with %s" testId (show err))
        Right (Right (Right _res')) -> do
          -- dumpVerifyScriptResult Nothing True res'
          pure ()
        Left err ->
          assertFailure (printf "%s: failed with %s" testId (show err))

    verifyTxNotApproved :: (T.Text, Result) -> IO ()
    verifyTxNotApproved (testId, res) =
      case res of
        Left _ -> pure ()
        Right (Left _err) -> pure ()
        Right (Right (Left (_, _))) -> pure ()
        Right (Right _res'@(Right _)) -> do
          -- let displayOpts = defaultDisplayOpts {showMetrics = True}
          -- dumpVerifyScriptResult displayOpts res'
          assertFailure (printf "%s: passed validation" testId)

    tryTest :: VmParams -> LibAuthTestRecord -> IO ()
    tryTest vmParams test = do
      (testId, res) <- runTest vmParams test
      case res of
        Right (Left err) -> do
          printf "\"%s\" -- validation failure %s\n" testId (show err)
        Right (Right (Left (err, _))) -> do
          printf "\"%s\" -- failed with %s\n" testId (show err)
        Right (Right (Right _res')) -> do
          printf "\"%s\" -- passed validation.\n" test.shortId
        Left err ->
          printf "\"%s\" -- failure %s\n" testId (show err)
      1 @?= (1 :: Int)

bch2025StandardFiles :: [String]
bch2025StandardFiles =
  (\x -> "bch_2025_standard" </> x <.> "json")
    <$> [ "core.benchmarks.arithmetic.add-sub.vmb_tests",
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

bch2025NonStandardFiles :: [String]
bch2025NonStandardFiles =
  (\x -> "bch_2025_nonstandard" </> x <.> "json")
    <$> [ "core.benchmarks.arithmetic.add-sub.vmb_tests",
          "core.benchmarks.arithmetic.div-mod.vmb_tests",
          "core.benchmarks.arithmetic.mul.vmb_tests",
          "core.benchmarks.hashing-bytes.packed.vmb_tests",
          "core.benchmarks.hashing-iters.packed.vmb_tests",
          "core.benchmarks.hashing.vmb_tests",
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
          "core.signing-serialization.vmb_tests"
        ]

bch2025InvalidFiles :: [String]
bch2025InvalidFiles =
  (\x -> "bch_2025_invalid" </> x <.> "json")
    <$> [ "core.benchmarks.bitwise.vmb_tests",
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
