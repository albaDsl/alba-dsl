-- Copyright (c) 2025 albaDsl

module TestStackBranches (testStackBranches) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Bch2025
  ( VmStack,
    b2SeUnsafe,
    boolToStackElement,
    i2SeUnsafe,
  )
import Data.Sequence ((|>))
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProgWithStack, getStacks)

testStackBranches :: TestTree
testStackBranches =
  testGroup
    "Stack Branches"
    [ testCase "Simple Stack Branch" $
        let (s, alt) =
              getStacks $
                evaluateProgWithStack
                  progSimpleStackBranch
                  ( S.fromList
                      [ boolToStackElement False,
                        boolToStackElement False,
                        i2SeUnsafe 30,
                        i2SeUnsafe 2,
                        b2SeUnsafe "hello"
                      ],
                    S.empty
                  )
         in (s, alt) @?= (S.fromList [i2SeUnsafe 1], S.empty),
      testCase "entry8 f0" $
        evalProgEntry8 0 @?= (S.fromList [i2SeUnsafe 101], S.empty),
      testCase "entry8 f1" $
        evalProgEntry8 1 @?= (S.fromList [i2SeUnsafe 103], S.empty),
      testCase "entry8 f2" $
        evalProgEntry8 2 @?= (S.fromList [i2SeUnsafe 106], S.empty),
      testCase "entry8 f3" $
        evalProgEntry8 3 @?= (S.fromList [i2SeUnsafe 110], S.empty),
      testCase "entry8 f4" $
        evalProgEntry8 4 @?= (S.fromList [i2SeUnsafe 115], S.empty),
      testCase "entry8 f5" $
        evalProgEntry8 5 @?= (S.fromList [i2SeUnsafe 121], S.empty),
      testCase "entry8 f6" $
        evalProgEntry8 6 @?= (S.fromList [i2SeUnsafe 128], S.empty),
      testCase "entry8 f7" $
        evalProgEntry8 7 @?= (S.fromList [i2SeUnsafe 136], S.empty)
    ]
  where
    evalProgEntry8 :: Integer -> (VmStack, VmStack)
    evalProgEntry8 fIdx =
      getStacks $
        evaluateProgWithStack
          progEntry8
          (startStack (succ fIdx) fIdx, S.empty)

    startStack :: Integer -> Integer -> VmStack
    startStack count fIdx =
      (i2SeUnsafe <$> S.fromList [1 .. count])
        |> i2SeUnsafe fIdx
        |> i2SeUnsafe 100

-- #choice has to be positioned above the stack branch. Otherwise it can't be
-- fetched given that the stack branch depth is unknown.
progSimpleStackBranch ::
  Fn
    ( s
        > ( (Base > N "b1" TBool)
              :| (Base > N "b2" TBool > N "b3" TBool > N "int" TInt)
          )
        > N "choice" TNat
        > N "bytes" TBytes
    )
    (s > TInt)
progSimpleStackBranch =
  begin
    ∘ (roll #bytes ∘ opSize ∘ op5 ∘ opNumEqualVerify ∘ opDrop)
    ∘ (roll #choice ∘ op1 ∘ opNumEqual)
    ∘ opIf
      (branch1 ∘ del #b1 ∘ int 2)
      (branch2 ∘ del #b2 ∘ del #b3 ∘ roll #int ∘ int 29 ∘ opSub)

{- ORMOLU_DISABLE -}
type Args0 = '[N "x0" TInt]
type Args1 = Append Args0 '[N "x1" TInt]
type Args2 = Append Args1 '[N "x2" TInt]
type Args3 = Append Args2 '[N "x3" TInt]
type Args4 = Append Args3 '[N "x4" TInt]
type Args5 = Append Args4 '[N "x5" TInt]
type Args6 = Append Args5 '[N "x6" TInt]
type Args7 = Append Args6 '[N "x7" TInt]
type Param = N "param" TInt
{- ORMOLU_ENABLE -}

progEntry8 ::
  Fn
    ( s
        > (Args0 :| Args1 :| Args2 :| Args3 :| Args4 :| Args5 :| Args6 :| Args7)
        > N "_fIdx" TInt
        > Param
    )
    (s > TInt)
progEntry8 = entry8 f0 f1 f2 f3 f4 f5 f6 f7
  where
    f0 :: Fn (Append s (Append Args0 '[Param])) (s > TInt)
    f0 =
      begin
        ∘ (roll #x0 ∘ roll #param)
        ∘ opAdd
    f1 :: Fn (Append s (Append Args1 '[Param])) (s > TInt)
    f1 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #param)
        ∘ (opAdd ∘ opAdd)
    f2 :: Fn (Append s (Append Args2 '[Param])) (s > TInt)
    f2 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #x2 ∘ roll #param)
        ∘ (opAdd ∘ opAdd ∘ opAdd)
    f3 :: Fn (Append s (Append Args3 '[Param])) (s > TInt)
    f3 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #x2 ∘ roll #x3)
        ∘ roll #param
        ∘ (opAdd ∘ opAdd ∘ opAdd ∘ opAdd)
    f4 :: Fn (Append s (Append Args4 '[Param])) (s > TInt)
    f4 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #x2 ∘ roll #x3)
        ∘ (roll #x4 ∘ roll #param)
        ∘ (opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd)
    f5 :: Fn (Append s (Append Args5 '[Param])) (s > TInt)
    f5 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #x2 ∘ roll #x3)
        ∘ (roll #x4 ∘ roll #x5 ∘ roll #param)
        ∘ (opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd)
    f6 :: Fn (Append s (Append Args6 '[Param])) (s > TInt)
    f6 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #x2 ∘ roll #x3)
        ∘ (roll #x4 ∘ roll #x5 ∘ roll #x6 ∘ roll #param)
        ∘ (opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd)
    f7 =
      begin
        ∘ (roll #x0 ∘ roll #x1 ∘ roll #x2 ∘ roll #x3)
        ∘ (roll #x4 ∘ roll #x5 ∘ roll #x6 ∘ roll #x7)
        ∘ roll #param
        ∘ (opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd ∘ opAdd)
