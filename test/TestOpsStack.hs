-- Copyright (c) 2025 albaDsl

module TestOpsStack (testOpsStack) where

import Alba.Dsl.V1.Bch2026
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils (evaluateProg, isTrue)

testOpsStack :: TestTree
testOpsStack =
  testGroup
    "Stack Ops"
    [ testCase "Dup, Drop, Nip, Over" $
        isTrue (evaluateProg progDupDropNipOver),
      testCase "Rot, Swap, Tuck" $ isTrue (evaluateProg progRotSwapTuck),
      testCase "2Dup, 2Drop, 2Over" $ isTrue (evaluateProg prog2Dup2Drop2Over),
      testCase "Alt stack" $ isTrue (evaluateProg progAltStack),
      testCase "Stack ops on two different types" $
        isTrue (evaluateProg progDifferentTypes),
      testCase "OpPick" $ isTrue (evaluateProg progPick),
      testCase "OpRoll" $ isTrue (evaluateProg progRoll)
    ]

progDupDropNipOver :: Fn s (s :> TBool)
progDupDropNipOver =
  begin
    ∘ one
    ∘ opFalse
    ∘ opNip
    ∘ opDup
    ∘ opDrop
    ∘ opFalse
    ∘ opEqual
  where
    one = op1 :: Fn s (s :> TNat)

progRotSwapTuck :: Fn s (s :> TBool)
progRotSwapTuck =
  begin
    ∘ opTrue ---- t
    ∘ one ------- t 1
    ∘ op2 ------- t 1 2
    ∘ opRot ----- 1 2 t
    ∘ opTuck ---- 1 t 2 t
    ∘ opVerify -- 1 t 2
    ∘ opSwap ---- 1 2 t
    ∘ opVerify -- 1 2
    ∘ opSwap ---- 2 1
    ∘ opDrop
    ∘ int 2
    ∘ opNumEqual
  where
    one = op1 :: Fn s (s :> TNat)

prog2Dup2Drop2Over :: Fn s (s :> TBool)
prog2Dup2Drop2Over =
  begin
    ∘ op1 ------- 1
    ∘ op2 ------- 1 2
    ∘ op2Dup ---- 1 2 1 2
    ∘ three ----- 1 2 1 2 3
    ∘ four ------ 1 2 1 2 3 4
    ∘ op2Over --- 1 2 1 2 3 4 1 2
    ∘ op2Drop --- 1 2 1 2 3 4
    ∘ op2Drop --- 1 2 1 2
    ∘ op2Drop --- 1 2
    ∘ opAdd ----- 3
    ∘ int 3
    ∘ opNumEqual
  where
    three = op3 :: Fn s (s :> TNat)
    four = op4 :: Fn s (s :> TNat)

progAltStack :: Fn s (s :> TBool)
progAltStack =
  begin
    ∘ op1
    ∘ opToAltStack
    ∘ op2
    ∘ opFromAltStack
    ∘ opAdd
    ∘ int 3
    ∘ opNumEqual

progDifferentTypes :: Fn s (s :> TBool)
progDifferentTypes =
  begin
    ∘ int 1
    ∘ opFalse
    ∘ opSwap
    ∘ opNip
    ∘ int 1
    ∘ opNumEqual

progPick :: Fn s (s :> TBool)
progPick =
  begin
    ∘ one
    ∘ two
    ∘ opFalse
    ∘ three
    ∘ opPick 2
    ∘ opNip
    ∘ opNip
    ∘ opNip
    ∘ opNip
    ∘ two
    ∘ opNumEqual
  where
    one = op1 :: Fn s (s :> TNat)
    two = op2 :: Fn s (s :> TNat)
    three = op3 :: Fn s (s :> TNat)

progRoll :: Fn s (s :> TBool)
progRoll =
  begin
    ∘ opFalse
    ∘ op1
    ∘ op2
    ∘ op3
    ∘ opRoll 3
    ∘ opDrop
    ∘ (opAdd ∘ opAdd ∘ int 6 ∘ opNumEqual)

-- Trying to access past the known stack won't compile.
-- accessPastKnownStack ::
--   Fn
--     (s :> TBytes :> TBool :> TBool :> TBool)
--     (s :> TNat)
-- accessPastKnownStack =
--   begin
--     ∘ opRoll @4
--     ∘ opDrop
--     ∘ opDrop
--     ∘ opDrop
--     ∘ opDrop
--     ∘ op1
