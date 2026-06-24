-- Copyright (c) 2025 albaDsl

module TestArguments (testArguments) where

import Alba.Dsl.V1.Bch2026
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils (evaluateProg, isTrue)

testArguments :: TestTree
testArguments =
  testGroup
    "Arguments"
    [ testCase "Args - Unnamed at call site" $
        isTrue (evaluateProg progUnnamedArgsAtCallSite),
      testCase "Args - Named at call site" $
        isTrue (evaluateProg progNamedArgsAtCallSite),
      testCase "pick inside if" $ isTrue (evaluateProg progIfArgPick),
      testCase "roll / del" $ isTrue (evaluateProg progArgRollDrop),
      testCase "Naming stack items" $ isTrue (evaluateProg namingStackItems),
      testCase "Duplicate name" $ isTrue (evaluateProg duplicateName)
    ]

-- Calling a function that expects named arguments without naming them at the
-- call site.
progUnnamedArgsAtCallSite :: Fn s (s :> TBool)
progUnnamedArgsAtCallSite =
  begin
    ∘ nat 2
    ∘ nat 3
    ∘ unname 2 calculateProperties
    ∘ (nat 6 ∘ opNumEqual)
    ∘ opSwap
    ∘ (nat 10 ∘ opNumEqual)
    ∘ opBoolAnd

-- Calling a function that expects named arguments and also naming them at the
-- call site.
progNamedArgsAtCallSite :: Fn s (s :> TBool)
progNamedArgsAtCallSite =
  begin
    ∘ name #"does-not-interfere" (nat 2)
    ∘ name #w (nat 1 ∘ nat 1 ∘ opAdd)
    ∘ name #h (nat 3)
    ∘ calculateProperties
    ∘ del #"does-not-interfere"
    ∘ (nat 6 ∘ opNumEqual)
    ∘ opSwap
    ∘ (nat 10 ∘ opNumEqual)
    ∘ opBoolAnd

-- Function that expects named arguments. Also calls other functions that may or
-- may not expect named arguments.
calculateProperties ::
  Fn
    (s :> N "w" TNat :> N "h" TNat)
    (s :> TNat :> TNat)
calculateProperties =
  begin
    ∘ (pickN #w ∘ pickN #h)
    ∘ perimeter
    ∘ (pick #w ∘ pick #h)
    ∘ area
    ∘ (del #h ∘ del #w)
  where
    area :: Fn (s :> TNat :> TNat) (s :> TNat)
    area = opMul

    perimeter :: Fn (s :> N "w" TNat :> N "h" TNat) (s :> TNat)
    perimeter =
      begin
        ∘ (pick #w ∘ roll #w ∘ opAdd)
        ∘ (pick #h ∘ roll #h ∘ opAdd)
        ∘ opAdd

type MiscArgs s =
  s :> N "x1" TNat :> N "x2" TNat :> N "x3" TBool :> N "x4" TBool :> N "x5" TNat

-- Exercising pick inside if statement.
progIfArgPick :: Fn s (s :> TBool)
progIfArgPick = nat 2 ∘ nat 4 ∘ opTrue ∘ opFalse ∘ nat 6 ∘ unname 5 f
  where
    f :: Fn (MiscArgs s) (s :> TBool)
    f =
      begin
        ∘ opTrue
        ∘ opIf
          (pick #x2 ∘ pick #x5)
          (pick #x1 ∘ pick #x2)
        ∘ opAdd
        ∘ delCount 5
        ∘ nat 10
        ∘ opNumEqual

-- Exercising del / roll with various types on the stack.
progArgRollDrop :: Fn s (s :> TBool)
progArgRollDrop = nat 2 ∘ nat 4 ∘ opTrue ∘ opFalse ∘ nat 6 ∘ unname 5 f
  where
    f :: Fn (MiscArgs s) (s :> TBool)
    f =
      begin
        ∘ (del #x3 ∘ del #x4 ∘ del #x1)
        ∘ (roll #x2 ∘ roll #x5)
        ∘ opAdd
        ∘ nat 10
        ∘ opNumEqual

-- Using name as a form of let-expression. Also returning a named stack item.
namingStackItems :: Fn s (s :> TBool)
namingStackItems =
  begin
    ∘ momentum
    ∘ pick #momentum
    ∘ del #momentum
    ∘ int 1250
    ∘ opNumEqual
  where
    momentum :: Fn s (s :> N "momentum" TInt)
    momentum =
      begin
        ∘ name #mass (int 100)
        ∘ name #v (int 5)
        ∘ name #"v^2" (pick #v ∘ roll #v ∘ opMul)
        ∘ name
          #momentum
          (roll #mass ∘ roll #"v^2" ∘ opMul ∘ int 2 ∘ opDiv)

-- Currently possible to have the same name in scope for more than one stack
-- item. Avoid.
duplicateName :: Fn s (s :> TBool)
duplicateName =
  begin
    ∘ nat 10
    ∘ nat 5
    ∘ unname 2 divide
    ∘ nat 2
    ∘ opNumEqual
  where
    divide ::
      Fn
        (s :> N "n1" TNat :> N "n1" TNat)
        (s :> TNat)
    divide =
      begin
        ∘ (roll #n1)
        ∘ (roll #n1)
        ∘ opSwap
        ∘ opDiv
