-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestOpsStack (testOpsStack) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common (i2SeUnsafe)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProg)

testOpsStack :: TestTree
testOpsStack =
  testGroup
    "Stack Ops"
    [ testCase "Dup, Drop, Nip, Over" $
        let Right (s, alt) = evaluateProg progDupDropNipOver
         in (s, alt) @?= (S.singleton (i2SeUnsafe 0), S.empty),
      testCase "Rot, Swap, Tuck" $
        let Right (s, alt) = evaluateProg progRotSwapTuck
         in (s, alt) @?= (S.singleton (i2SeUnsafe 2), S.empty),
      testCase "2Dup, 2Drop, 2Over" $
        let Right (s, alt) = evaluateProg prog2Dup2Drop2Over
         in (s, alt) @?= (S.singleton (i2SeUnsafe 3), S.empty),
      testCase "Alt stack" $
        let Right (s, alt) = evaluateProg progAltStack
         in (s, alt) @?= (S.singleton (i2SeUnsafe 3), S.empty),
      testCase "Stack ops on two different types" $
        let Right (s, alt) = evaluateProg progDifferentTypes
         in (s, alt) @?= (S.singleton (i2SeUnsafe 1), S.empty),
      testCase "OpPick" $
        let Right (s, alt) = evaluateProg progPick
         in (s, alt) @?= (S.singleton (i2SeUnsafe 2), S.empty),
      testCase "OpRoll" $
        let Right (s, alt) = evaluateProg progRoll
         in (s, alt)
              @?= ( S.fromList [i2SeUnsafe 1, i2SeUnsafe 2, i2SeUnsafe 3],
                    S.empty
                  )
    ]

progDupDropNipOver :: FN s (s > TBool)
progDupDropNipOver =
  begin
    # one
    # opFalse
    # opNip
    # opDup
    # opDrop
  where
    one = op1 :: FN s (s > TNat)

progRotSwapTuck :: FN s (s > TNat)
progRotSwapTuck =
  begin
    # opTrue ---- t
    # one ------- t 1
    # op2 ------- t 1 2
    # opRot ----- 1 2 t
    # opTuck ---- 1 t 2 t
    # opVerify -- 1 t 2
    # opSwap ---- 1 2 t
    # opVerify -- 1 2
    # opSwap ---- 2 1
    # opDrop
  where
    one = op1 :: FN s (s > TNat)

prog2Dup2Drop2Over :: FN s (s > TNat)
prog2Dup2Drop2Over =
  begin
    # op1 ------- 1
    # op2 ------- 1 2
    # op2Dup ---- 1 2 1 2
    # three ----- 1 2 1 2 3
    # four ------ 1 2 1 2 3 4
    # op2Over --- 1 2 1 2 3 4 1 2
    # op2Drop --- 1 2 1 2 3 4
    # op2Drop --- 1 2 1 2
    # op2Drop --- 1 2
    # opAdd ----- 3
  where
    three = op3 :: FN s (s > TNat)
    four = op4 :: FN s (s > TNat)

progAltStack :: FN s (s > TNat)
progAltStack =
  begin
    # op1
    # opToAltStack
    # op2
    # opFromAltStack
    # opAdd

progDifferentTypes :: FN s (s > TNat)
progDifferentTypes =
  begin
    # op1
    # opFalse
    # opSwap
    # opNip

progPick :: FN s (s > TNat)
progPick =
  begin
    # one
    # two
    # opFalse
    # three
    # opPick @2
    # opNip
    # opNip
    # opNip
    # opNip
  where
    one = op1 :: FN s (s > TNat)
    two = op2 :: FN s (s > TNat)
    three = op3 :: FN s (s > TNat)

progRoll :: FN s (s > TNat > TNat > TNat)
progRoll =
  begin
    # opFalse
    # op1
    # op2
    # op3
    # opRoll @3
    # opDrop

-- Trying to access past the known stack won't compile.
-- accessPastKnownStack ::
--   FN
--     (s > TBytes > TBool > TBool > TBool)
--     (s > TNat)
-- accessPastKnownStack =
--   begin
--     # opRoll @4
--     # opDrop
--     # opDrop
--     # opDrop
--     # opDrop
--     # op1
