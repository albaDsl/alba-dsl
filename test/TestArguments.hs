-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestArguments (testArguments) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common (i2SeUnsafe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProg)

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
        "argPick inside if"
        $ let Right (s, alt) = evaluateProg progIfArgPick
           in (s, alt) @?= (S.fromList [i2SeUnsafe 10], S.empty),
      testCase
        "argRoll / argDrop"
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
    # argDrop @"does-not-interfere"

-- Function that expects named arguments. Also calls other functions that may or
-- may not expect named arguments.
calculateProperties ::
  FN
    (s > N "w" TNat > N "h" TNat)
    (s > TNat > TNat)
calculateProperties =
  begin
    # (argPickN @"w" # argPickN @"h")
    # perimeter
    # (argPick @"w" # argPick @"h")
    # area
    # argsDrop @2
  where
    area :: FN (s > TNat > TNat) (s > TNat)
    area = opMul

    perimeter :: FN (s > N "w" TNat > N "h" TNat) (s > TNat)
    perimeter =
      begin
        # (argPick @"w" # argRoll @"w" # opAdd)
        # (argPick @"h" # argRoll @"h" # opAdd)
        # opAdd

type MiscArgs s =
  s > N "x1" TNat > N "x2" TNat > N "x3" TBool > N "x4" TBool > N "x5" TNat

-- Exercising argPick inside if statement.
progIfArgPick :: FN s (s > TNat)
progIfArgPick = nat 2 # nat 4 # opTrue # opFalse # nat 6 # unname @5 f
  where
    f :: FN (MiscArgs s) (s > TNat)
    f =
      begin
        # opTrue
        # opIf
          (argPick @"x2" # argPick @"x5")
          (argPick @"x1" # argPick @"x2")
        # opAdd
        # argsDrop @5

-- Exercising argDrop / argRoll with various types on the stack.
progArgRollDrop :: FN s (s > TNat)
progArgRollDrop = nat 2 # nat 4 # opTrue # opFalse # nat 6 # unname @5 f
  where
    f :: FN (MiscArgs s) (s > TNat)
    f =
      begin
        # (argDrop @"x3" # argDrop @"x4" # argDrop @"x1")
        # (argRoll @"x2" # argRoll @"x5")
        # opAdd

-- Using name as a form of let-expression. Also returning a named stack item.
namingStackItems :: FN s (s > TInt)
namingStackItems =
  begin
    # momentum
    # argPick @"momentum"
    # argDrop @"momentum"
  where
    momentum :: FN s (s > N "momentum" TInt)
    momentum =
      begin
        # name @"mass" (int 100)
        # name @"v" (int 5)
        # name @"v^2" (argPick @"v" # argRoll @"v" # opMul)
        # name @"momentum"
          (argRoll @"mass" # argRoll @"v^2" # opMul # int 2 # opDiv)

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
        # (argRoll @"n1")
        # (argRoll @"n1")
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
--     # argsDrop @2
--     # op1
