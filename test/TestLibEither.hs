-- Copyright (c) 2026 albaDsl

module TestLibEither (testLibEither) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.BlobEq (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.Error (error)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, rot, swap)
import Alba.Dsl.V1.Bch2026.Contract.TBytes128 (bytes128)
import Alba.Dsl.V1.Bch2026.Contract.TEither
  ( TEither,
    either,
    ifLeft,
    isLeft,
    isRight,
    left,
    right,
  )
import Alba.Dsl.V1.Bch2026.Contract.TInt64 (int64)
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8, int8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude (($))

testLibEither :: TestTree
testLibEither =
  testGroup
    "Either"
    [ testCase "Basics" $ do isTrue (evaluateProg progBasics)
    ]

progBasics :: Fn s (s :> TBool)
progBasics =
  begin
    ∘ ( begin
          ∘ (int8 1 ∘ testIsLeftIsRight)
          ∘ (int64 1024 ∘ testIsLeftIsRight)
          ∘ (bytes128 "hello world" ∘ testIsLeftIsRight)
      )
    ∘ ( begin
          ∘ (int8 1 ∘ testIfLeft)
          ∘ (int64 1024 ∘ testIfLeft)
          ∘ (bytes128 "hello world" ∘ testIfLeft)
      )
    ∘ ( begin
          ∘ (int8 1 ∘ testEither)
          ∘ (int64 1024 ∘ testEither)
          ∘ (bytes128 "hello world" ∘ testEither)
      )
    ∘ opTrue
  where
    testIsLeftIsRight :: (StackEntry a) => Fn (s :> a) s
    testIsLeftIsRight =
      begin
        ∘ (dup ∘ left ∘ isLeft ∘ opVerify)
        ∘ (dup ∘ right ∘ isLeft ∘ opNot ∘ opVerify)
        ∘ (dup ∘ left ∘ isRight ∘ opNot ∘ opVerify)
        ∘ (right ∘ isRight ∘ opVerify)

    testIfLeft :: (StackEntry a) => Fn (s :> a) s
    testIfLeft =
      begin
        ∘ (dup ∘ r ∘ ifLeft fail drop)
        ∘ (l ∘ ifLeft drop fail)
      where
        l :: Fn (s :> a) (s :> TEither a TInt8)
        l = left

        r :: Fn (s :> a) (s :> TEither TInt8 a)
        r = right

    fail :: FnA s alt s' alt'
    fail = bytes "Fail" ∘ error

    testEither :: (StackEntry a) => Fn (s :> a) s
    testEither =
      begin
        ∘ dup
        ∘ (r ∘ quot1 (drop ∘ int 1) ∘ quot1 (drop ∘ int 2) ∘ rot ∘ either)
        ∘ swap
        ∘ (l ∘ quot1 (drop ∘ int 2) ∘ quot1 (drop ∘ int 1) ∘ rot ∘ either)
        ∘ (opAdd ∘ int 4 ∘ equalVerify)
      where
        l :: Fn (s :> a) (s :> TEither a TInt8)
        l = left

        r :: Fn (s :> a) (s :> TEither TInt8 a)
        r = right
