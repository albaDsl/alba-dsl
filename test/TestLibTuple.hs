-- Copyright (c) 2025 albaDsl

module TestLibTuple (testLibTuple) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Int64 (int64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (int8)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (fst, snd, tuple, untuple)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude hiding (drop, fst, snd)

testLibTuple :: TestTree
testLibTuple =
  testGroup
    "Tuple"
    [ testCase "Basics" $ do isTrue (evaluateProg progBasics),
      testCase "Nested" $ do isTrue (evaluateProg progNested)
    ]

progBasics :: FN s (s > TBool)
progBasics =
  begin
    # ( begin
          # ( begin
                # (int8 1 # int64 2 # tuple)
                # (untuple # opDrop # int8 1 # opEqualVerify)
            )
          # ( begin
                # (int8 1 # int64 2 # tuple)
                # (untuple # opNip # int64 2 # opEqualVerify)
            )
          # ( begin
                # (int8 1 # int64 2 # tuple # fst # int8 1 # opEqualVerify)
                # (int8 1 # int64 2 # tuple # snd # int64 2 # opEqualVerify)
            )
      )
    # opTrue

progNested :: FN s (s > TBool)
progNested =
  begin
    # ( begin
          # ( begin
                # (int64 1 # int8 2 # int64 3 # tuple # tuple)
                # (snd # fst # int8 2 # opEqualVerify)
            )
      )
    # opTrue
