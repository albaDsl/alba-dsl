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
import Alba.Vm.Common
  ( ScriptError,
    VmStack,
    i2SeUnsafe,
    stackElementToBytes,
    stackElementToInteger,
  )
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Word (Word8)
import DslDemo.Exponentiation (pow)
import DslDemo.MergeSort (sort)
import QuickCheckSupport (AsciiString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (==>))

testEval :: TestTree
testEval =
  testGroup
    "Eval"
    [ testCase
        "Eval basics 1"
        $ let res = evaluateProg progBasics1
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 6], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Eval basics 2"
        $ let res = evaluateProg progBasics2
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 15], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Nested eval"
        $ let res = evaluateProg progNested
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 9], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Recursion — factorial"
        $ let res = evaluateProg progFactorial
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 720], S.empty)
                Left err -> error ("err: " <> show err),
      testProperty "Recursion — pow" propPow,
      testProperty "Recursion — merge sort" propSort
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

progFactorial :: FN s (s > TInt)
progFactorial = int 6 # lambda' fac # recur fac
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
  let res = evaluateProg (int (fromIntegral b) # nat (fromIntegral n) # pow)
   in case res of
        Right (_ S.:|> x, _alt) ->
          let y =
                fromRight
                  (error "propPow")
                  (stackElementToInteger vmParamsStandard x)
              y' = (fromIntegral b :: Integer) ^ (fromIntegral n :: Integer)
           in y == y'
        _ -> False

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 20) ==>
    let res = evaluateProg (bytes xs # sort)
     in case res of
          Right (_ S.:|> x, _alt) -> stackElementToBytes x == B.sort xs
          _ -> False

evaluateProg :: FNA s '[] s' alt' -> Either ScriptError (VmStack, VmStack)
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Left (err, _) -> Left err
        Right VmState {s, alt} -> Right (s, alt)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
