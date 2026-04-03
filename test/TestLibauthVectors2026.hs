-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestLibauthVectors2026 (testLibauthVectors2026) where

import Control.Monad (unless)
import Data.Text qualified as T
import LibauthSupport
  ( LibauthTest (..),
    TestMode (..),
    findAndLoad,
    printSummary,
    runTest,
    tryTest,
    verifyResult,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestLibauthVectorsExclusions2026
  ( excludeInvalid,
    excludeNonStandardInNonStandardMode,
    excludeNonStandardInStandardMode,
    excludeStandard,
  )
import Text.Printf (printf)

testLibauthVectors2026 :: TestTree
testLibauthVectors2026 =
  testGroup
    "Libauth vectors (Bch2026)"
    [ testCase "bch_2026_standard in standard-mode" $ do
        tests <- standardTests >>= filterTests (`notElem` excludeStandard)
        mapM_ (runAndVerify Standard2026) tests,
      testCase "bch_2026_standard in nonstandard-mode" $ do
        tests <- standardTests >>= filterTests (`notElem` excludeStandard)
        mapM_ (runAndVerify Nonstandard2026) tests,
      testCase "bch_2026_nonstandard in standard-mode" $ do
        tests <-
          nonStandardTests
            >>= filterTests
              (`notElem` excludeNonStandardInStandardMode)
        mapM_ (runAndVerify Standard2026) tests,
      testCase "bch_2026_nonstandard in nonstandard-mode" $ do
        tests <- nonStandardTests >>= filterTests (const True)
        mapM_ (runAndVerify Nonstandard2026) tests,
      testCase "bch_2026_invalid in standard-mode" $ do
        tests <- invalidTests >>= filterTests (`notElem` excludeInvalid)
        mapM_ (runAndVerify Standard2026) tests,
      testCase "bch_2026_invalid in nonstandard-mode" $ do
        tests <- invalidTests >>= filterTests (`notElem` excludeInvalid)
        mapM_ (runAndVerify Nonstandard2026) tests
    ]
  where
    filterTests :: (T.Text -> Bool) -> [LibauthTest] -> IO [LibauthTest]
    filterTests check tests = do
      let tests' = filter (\test -> check test.shortId) tests
      printSummary tests' tests
      pure tests'

    runAndVerify mode test = runTest mode test >>= verifyResult mode test

    -- runAndVerify' = tryTest

    standardTests = findAndLoad "bch_2026_standard" 69 14283

    nonStandardTests = findAndLoad "bch_2026_nonstandard" 41 2289

    invalidTests = findAndLoad "bch_2026_invalid" 54 13159
