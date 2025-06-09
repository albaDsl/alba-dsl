-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestEval (testEval) where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (i2SeUnsafe)
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Word (Word8)
import DslDemo.Exponentiation (pow)
import DslDemo.MergeSort.MergeSortEval (sort)
import QuickCheckSupport (AsciiString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (==>))

testEval :: TestTree
testEval =
  testGroup
    "Eval"
    [ testCase "Eval basics 1" $ evaluateProg progBasics1 @?= True,
      testCase "Eval basics 2" $ evaluateProg progBasics2 @?= True,
      testCase "Nested eval" $ evaluateProg progNested @?= True,
      testCase "Recursion — factorial" $ evaluateProg progFactorial @?= True,
      testProperty "Recursion — pow" propPow,
      testProperty "Recursion — merge sort" propSort
    ]

progBasics1 :: FN s (s > TBool)
progBasics1 =
  begin
    # lambda (op1 # op2 # op3)
    # opEval
    # opAdd
    # opAdd
    # nat 6
    # opNumEqual

progBasics2 :: FN s (s > TBool)
progBasics2 =
  begin
    # int 5
    # lambda f
    # opEval
    # int 15
    # opNumEqual
  where
    -- A function that calculates x^2 - 2*x
    f :: S (s > TInt) alt -> S (s > TInt) alt
    f = opDup # square # opSwap # coeff 2 # opSub
      where
        square = opDup # opMul
        coeff c = int c # opMul

progNested :: FN s (s > TBool)
progNested =
  begin
    # lambda (op1 # op2 # lambda (opAdd # opDup # opMul) # opEval)
    # opEval
    # int 9
    # opNumEqual

progFactorial :: FN s (s > TBool)
progFactorial =
  begin
    # (int 0 # lambda' fac # recur fac # int 1 # opNumEqual)
    # (int 1 # lambda' fac # recur fac # int 1 # opNumEqual)
    # (int 6 # lambda' fac # recur fac # int 720 # opNumEqual)
    # (int 10 # lambda' fac # recur fac # int 3_628_800 # opNumEqual)
    # (opBoolAnd # opBoolAnd # opBoolAnd)
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

propPow :: Int -> Word8 -> Bool
propPow b n =
  let expected = (fromIntegral b :: Integer) ^ (fromIntegral n :: Integer)
      prog =
        begin
          # int (fromIntegral b)
          # nat (fromIntegral n)
          # pow
          # int expected
          # opNumEqual
   in evaluateProg prog

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 20) ==>
    evaluateProg (bytes xs # sort # bytes (B.sort xs) # opEqual)

evaluateProg :: FNA s '[] s' alt' -> Bool
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Right VmState {s, alt} ->
          s == S.fromList [i2SeUnsafe 1] && alt == S.empty
        Left (err, _) -> error ("err: " <> show err)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
