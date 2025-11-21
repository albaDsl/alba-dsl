-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestLibauthVectors2025 (testLibauthVectors2025) where

import Control.Monad (unless)
import Data.Text qualified as T
import LibauthSupport
  ( LibauthTest (..),
    TestMode (..),
    findAndLoad,
    printSummary,
    runTest,
    verifyResult,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestLibauthVectorsExclusions2025
  ( excludeInvalid,
    excludeNonStandardInStandardMode,
    excludeStandard,
  )
import Text.Printf (printf)

testLibauthVectors2025 :: TestTree
testLibauthVectors2025 =
  testGroup
    "Libauth vectors 2025"
    [ testCase "bch_2025_standard in standard-mode" $ do
        tests <- standardTests >>= filterTests (`notElem` excludeStandard)
        mapM_ (runAndVerify Standard2025) tests,
      testCase "bch_2025_standard in nonstandard-mode" $ do
        tests <- standardTests >>= filterTests (`notElem` excludeStandard)
        mapM_ (runAndVerify Nonstandard2025) tests,
      testCase "bch_2025_nonstandard in standard-mode" $ do
        tests <-
          nonStandardTests
            >>= filterTests
              (`notElem` excludeNonStandardInStandardMode)
        mapM_ (runAndVerify Standard2025) tests,
      testCase "bch_2025_nonstandard in nonstandard-mode" $ do
        tests <- nonStandardTests >>= filterTests (const True)
        mapM_ (runAndVerify Nonstandard2025) tests,
      testCase "bch_2025_invalid in standard-mode" $ do
        tests <- invalidTests >>= filterTests (`notElem` excludeInvalid)
        mapM_ (runAndVerify Standard2025) tests,
      testCase "bch_2025_invalid in nonstandard-mode" $ do
        tests <- invalidTests >>= filterTests (`notElem` excludeInvalid)
        -- tests <- invalidTests >>= filterTests (== "xxw95u")
        mapM_ (runAndVerify Nonstandard2025) tests
    ]
  where
    filterTests :: (T.Text -> Bool) -> [LibauthTest] -> IO [LibauthTest]
    filterTests check tests = do
      let tests' = filter (\test -> check test.shortId) tests
      printSummary tests' tests
      pure tests'

    runAndVerify mode test = runTest mode test >>= verifyResult mode test

    standardTests = findAndLoad "bch_2025_standard" 64 7873

    nonStandardTests = findAndLoad "bch_2025_nonstandard" 56 7976

    invalidTests = findAndLoad "bch_2025_invalid" 52 12603
