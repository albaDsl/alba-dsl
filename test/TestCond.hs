-- Copyright (c) 2025 albaDsl

module TestCond (testCond) where

import Alba.Dsl.V1.Bch2025
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils (evaluateProg, isTrue)

testCond :: TestTree
testCond =
  testGroup
    "Cond"
    [ testCase "Cond - Nats" $ isTrue (evaluateProg progCondNats),
      testCase "Cond - Strings" $ isTrue (evaluateProg progCondStrings),
      testCase "Cond - Strings - Default case" $
        isTrue (evaluateProg progCondStringsDefault),
      testCase "Cond - Nested" $ isTrue (evaluateProg progCondNested)
    ]

progCondNats :: Fn s (s :> TBool)
progCondNats =
  begin
    ∘ nat 2
    ∘ case'
      [ (is 1, nat 2),
        (is 2, nat 3),
        (is 3, nat 4)
      ]
      (nat 0)
    ∘ opNip
    ∘ nat 3
    ∘ opNumEqual
  where
    is x = nat x ∘ opNumEqual

progCondStrings :: Fn s (s :> TBool)
progCondStrings =
  begin
    ∘ bytes "orange"
    ∘ case'
      [ (is "apple", nat 1),
        (is "pear", nat 2),
        (is "orange", nat 3),
        (is "lemon", nat 4)
      ]
      (nat 0)
    ∘ opNip
    ∘ nat 3
    ∘ opNumEqual
  where
    is x = bytes x ∘ opEqual

progCondStringsDefault :: Fn s (s :> TBool)
progCondStringsDefault =
  begin
    ∘ bytes "strawberry"
    ∘ case'
      [ (is "apple", nat 1),
        (is "pear", nat 2),
        (is "orange", nat 3),
        (is "lemon", nat 4)
      ]
      (nat 0)
    ∘ opNip
    ∘ nat 0
    ∘ opNumEqual
  where
    is x = bytes x ∘ opEqual

progCondNested :: Fn s (s :> TBool)
progCondNested =
  begin
    ∘ nat 75
    ∘ case'
      [ (inRange 0 50, bytes "low"),
        ( inRange 50 100,
          case'
            [ (inRange 50 75, bytes "mid — first"),
              (inRange 75 100, bytes "mid — second")
            ]
            (bytes "failure")
        ),
        (inRange 100 150, bytes "high")
      ]
      (bytes "failure")
    ∘ opNip
    ∘ bytes "mid — second"
    ∘ opEqual
  where
    inRange x y = nat x ∘ nat y ∘ opWithin
