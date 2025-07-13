-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestArguments (testArguments) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common (i2SeUnsafe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProg)
import Prelude hiding (drop)

testArguments :: TestTree
testArguments =
  testGroup
    "Arguments"
    [ testCase
        "Args — Unnamed at call site"
        $ let Right (s, alt) = evaluateProg progUnnamedArgsAtCallSite
           in (s, alt)
                @?= ( S.fromList [i2SeUnsafe 10, i2SeUnsafe 6],
                      S.empty
                    ),
      testCase
        "Args — Named at call site"
        $ let Right (s, alt) = evaluateProg progNamedArgsAtCallSite
           in (s, alt)
                @?= ( S.fromList [i2SeUnsafe 10, i2SeUnsafe 6],
                      S.empty
                    ),
      testCase
        "pick inside if"
        $ let Right (s, alt) = evaluateProg progIfArgPick
           in (s, alt) @?= (S.fromList [i2SeUnsafe 10], S.empty),
      testCase
        "roll / drop"
        $ let Right (s, alt) = evaluateProg progArgRollDrop
           in (s, alt) @?= (S.fromList [i2SeUnsafe 10], S.empty),
      testCase
        "Naming stack items"
        $ let Right (s, alt) = evaluateProg namingStackItems
           in (s, alt) @?= (S.fromList [i2SeUnsafe 1250], S.empty),
      testCase
        "Duplicate name"
        $ let Right (s, alt) = evaluateProg duplicateName
           in (s, alt) @?= (S.fromList [i2SeUnsafe 2], S.empty)
    ]

-- Calling a function that expects named arguments without naming them at the
-- call site.
progUnnamedArgsAtCallSite :: FN s (s > TNat > TNat)
progUnnamedArgsAtCallSite =
  begin
    # nat 2
    # nat 3
    # unname @2 calculateProperties

-- Calling a function that expects named arguments and also naming them at the
-- call site.
progNamedArgsAtCallSite :: FN s (s > TNat > TNat)
progNamedArgsAtCallSite =
  begin
    # name @"does-not-interfere" (nat 2)
    # name @"w" (nat 1 # nat 1 # opAdd)
    # name @"h" (nat 3)
    # calculateProperties
    # drop @"does-not-interfere"

-- Function that expects named arguments. Also calls other functions that may or
-- may not expect named arguments.
calculateProperties ::
  FN
    (s > N "w" TNat > N "h" TNat)
    (s > TNat > TNat)
calculateProperties =
  begin
    # (pickN @"w" # pickN @"h")
    # perimeter
    # (pick @"w" # pick @"h")
    # area
    # (drop @"h" # drop @"w")
  where
    area :: FN (s > TNat > TNat) (s > TNat)
    area = opMul

    perimeter :: FN (s > N "w" TNat > N "h" TNat) (s > TNat)
    perimeter =
      begin
        # (pick @"w" # roll @"w" # opAdd)
        # (pick @"h" # roll @"h" # opAdd)
        # opAdd

type MiscArgs s =
  s > N "x1" TNat > N "x2" TNat > N "x3" TBool > N "x4" TBool > N "x5" TNat

-- Exercising pick inside if statement.
progIfArgPick :: FN s (s > TNat)
progIfArgPick = nat 2 # nat 4 # opTrue # opFalse # nat 6 # unname @5 f
  where
    f :: FN (MiscArgs s) (s > TNat)
    f =
      begin
        # opTrue
        # opIf
          (pick @"x2" # pick @"x5")
          (pick @"x1" # pick @"x2")
        # opAdd
        # dropCount @5

-- Exercising drop / roll with various types on the stack.
progArgRollDrop :: FN s (s > TNat)
progArgRollDrop = nat 2 # nat 4 # opTrue # opFalse # nat 6 # unname @5 f
  where
    f :: FN (MiscArgs s) (s > TNat)
    f =
      begin
        # (drop @"x3" # drop @"x4" # drop @"x1")
        # (roll @"x2" # roll @"x5")
        # opAdd

-- Using name as a form of let-expression. Also returning a named stack item.
namingStackItems :: FN s (s > TInt)
namingStackItems =
  begin
    # momentum
    # pick @"momentum"
    # drop @"momentum"
  where
    momentum :: FN s (s > N "momentum" TInt)
    momentum =
      begin
        # name @"mass" (int 100)
        # name @"v" (int 5)
        # name @"v^2" (pick @"v" # roll @"v" # opMul)
        # name @"momentum"
          (roll @"mass" # roll @"v^2" # opMul # int 2 # opDiv)

-- Currently possible to have the same name in scope for more than one stack
-- item. Avoid.
duplicateName :: FN s (s > TNat)
duplicateName =
  begin
    # nat 10
    # nat 5
    # unname @2 divide
  where
    divide ::
      FN
        (s > N "n1" TNat > N "n1" TNat)
        (s > TNat)
    divide =
      begin
        # (roll @"n1")
        # (roll @"n1")
        # opSwap
        # opDiv

-- Trying to use opRoll on named args won't compile.
-- accessNamedArg ::
--   FN
--     (s > N "x1" TInt > N "x2" TInt)
--     (s > TNat)
-- accessNamedArg =
--   begin
--     # opRoll @0
--     # opRoll @1
--     # (drop @"x2" # drop @"x1")
--     # op1
