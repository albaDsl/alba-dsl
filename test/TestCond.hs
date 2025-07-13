-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestCond (testCond) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common.StackElement
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestUtils (evaluateProg)

testCond :: TestTree
testCond =
  testGroup
    "Cond"
    [ testCase "Cond — Nats" $
        case evaluateProg progCondNats of
          Right (s, alt) -> (s, alt) @?= (S.singleton $ i2SeUnsafe 1, S.empty)
          Left err -> assertFailure (show err),
      testCase "Cond — Strings" $
        case evaluateProg progCondStrings of
          Right (s, alt) -> (s, alt) @?= (S.singleton $ i2SeUnsafe 1, S.empty)
          Left err -> assertFailure (show err),
      testCase "Cond — Strings — Default case" $
        case evaluateProg progCondStringsDefault of
          Right (s, alt) -> (s, alt) @?= (S.singleton $ i2SeUnsafe 1, S.empty)
          Left err -> assertFailure (show err),
      testCase "Cond — Nested" $
        case evaluateProg progCondNested of
          Right (s, alt) -> (s, alt) @?= (S.singleton $ i2SeUnsafe 1, S.empty)
          Left err -> assertFailure (show err)
    ]

progCondNats :: FN s (s > TBool)
progCondNats =
  begin
    # nat 2
    # cond
      [ (is 1, nat 2),
        (is 2, nat 3),
        (is 3, nat 4)
      ]
      (nat 0)
    # opNip
    # nat 3
    # opNumEqual
  where
    is x = nat x # opNumEqual

progCondStrings :: FN s (s > TBool)
progCondStrings =
  begin
    # bytes "orange"
    # cond
      [ (is "apple", nat 1),
        (is "pear", nat 2),
        (is "orange", nat 3),
        (is "lemon", nat 4)
      ]
      (nat 0)
    # opNip
    # nat 3
    # opNumEqual
  where
    is x = bytes x # opEqual

progCondStringsDefault :: FN s (s > TBool)
progCondStringsDefault =
  begin
    # bytes "strawberry"
    # cond
      [ (is "apple", nat 1),
        (is "pear", nat 2),
        (is "orange", nat 3),
        (is "lemon", nat 4)
      ]
      (nat 0)
    # opNip
    # nat 0
    # opNumEqual
  where
    is x = bytes x # opEqual

progCondNested :: FN s (s > TBool)
progCondNested =
  begin
    # nat 75
    # cond
      [ (inRange 0 50, bytes "low"),
        ( inRange 50 100,
          cond
            [ (inRange 50 75, bytes "mid — first"),
              (inRange 75 100, bytes "mid — second")
            ]
            (bytes "failure")
        ),
        (inRange 100 150, bytes "high")
      ]
      (bytes "failure")
    # opNip
    # bytes "mid — second"
    # opEqual
  where
    inRange x y = nat x # nat y # opWithin
