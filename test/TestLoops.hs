-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestLoops (testLoops) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Math (factorial, pow)
import Alba.Dsl.V1.Bch2026.Contract.Prelude (iterate)
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (ScriptError, VmStack, i2SeUnsafe, stackElementToInteger)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (iterate)

testLoops :: TestTree
testLoops =
  testGroup
    "Loops"
    [ testCase
        "Loops — factorial 1"
        $ let res = evaluateProg progFactorial1
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 1], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Loops — factorial 2"
        $ let res = evaluateProg progFactorial2
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 1], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Loops — factorial 3"
        $ let res = evaluateProg progFactorial3
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 1], S.empty)
                Left err -> error ("err: " <> show err),
      testProperty "Loops — pow" propPow
    ]

progFactorial1 :: FN s (s > TBool)
progFactorial1 = progFacTest fac
  where
    fac :: Natural -> FN s (s > TNat)
    fac n = nat n # factorial

progFacTest :: (forall s'. Natural -> FN s' (s' > TNat)) -> FN s (s > TBool)
progFacTest fac =
  begin
    # (fac 0 # nat 1 # opNumEqual)
    # (fac 1 # nat 1 # opNumEqual)
    # (fac 6 # nat 720 # opNumEqual)
    # (fac 10 # nat 3_628_800 # opNumEqual)
    # (opBoolAnd # opBoolAnd # opBoolAnd)

progFactorial2 :: FN s (s > TBool)
progFactorial2 = progFacTest fac
  where
    fac :: Natural -> FN s (s > TNat)
    fac n = nat 1 # nat n # iterate n (unname @2 fn) # opDrop

    fn :: FN (s > N "product" TNat > N "n" TNat) (s > TNat > TNat)
    fn =
      begin
        # (argPick @"n" # argRoll @"product" # opMul)
        # (argRoll @"n" # op1SubUnsafe)

progFactorial3 :: FN s (s > TBool)
progFactorial3 = progFacTest fac
  where
    fac :: Natural -> FN s (s > TNat)
    fac n = nat 1 # iterate n (unname @1 fn)

    fn :: FNA (s > N "product" TNat) (alt > TNat) (s > TNat) (alt > TNat)
    fn =
      begin
        # (opFromAltStack # opDup # opToAltStack)
        # (argRoll @"product" # opMul)

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

evaluateProg :: FNA s '[] s' alt' -> Either ScriptError (VmStack, VmStack)
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Left (err, _) -> Left err
        Right VmState {s, alt} -> Right (s, alt)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
