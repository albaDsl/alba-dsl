-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestEval (testEval) where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2025 (i2SeUnsafe)
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (ScriptError, VmStack)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testEval :: TestTree
testEval =
  testGroup
    "Eval"
    [ testCase
        "Basics 1"
        $ let res = evaluateProg progBasics1
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 6], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Basics 2"
        $ let res = evaluateProg progBasics2
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 15], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Nested"
        $ let res = evaluateProg progNested
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 9], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Recursion"
        $ let res = evaluateProg progRecursion
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 720], S.empty)
                Left err -> error ("err: " <> show err)
    ]

progBasics1 :: FN s (s > TNat)
progBasics1 =
  begin
    # lambda (op1 # op2 # op3)
    # opEval
    # opAdd
    # opAdd

progBasics2 :: FN s (s > TInt)
progBasics2 =
  begin
    # int 5
    # lambda f
    # opEval
  where
    -- A function that calculates x^2 - 2*x
    f :: S (s > TInt) alt -> S (s > TInt) alt
    f = opDup # square # opSwap # coeff 2 # opSub
      where
        square = opDup # opMul
        coeff c = int c # opMul

progNested :: FN s (s > TNat)
progNested =
  begin
    # lambda (op1 # op2 # lambda (opAdd # opDup # opMul) # opEval)
    # opEval

-- Factorial.
progRecursion :: FN s (s > TInt)
progRecursion = int 6 # lambda' fac # recur fac
  where
    fac = unname @2 fac'

    fac' :: FN (s > N "n" TInt > N "rec" TLambdaUntyped) (s > TInt)
    fac' =
      begin
        # argPick @"n"
        # ifZero
          (int 1 # argsDrop @2)
          ( begin
              # argPick @"n"
              # ((argRoll @"n" # op1Sub) # argRoll @"rec" # recur fac)
              # opMul
          )

evaluateProg :: FNA s '[] s' alt' -> Either ScriptError (VmStack, VmStack)
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Left (err, _) -> Left err
        Right VmState {s, alt} -> Right (s, alt)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
