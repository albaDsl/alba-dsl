-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestStackBranches (testStackBranches) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Bch2025
  ( ScriptError,
    VmStack,
    b2SeUnsafe,
    boolToStackElement,
    i2SeUnsafe,
  )
import Data.Sequence ((|>))
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (evaluateProgWithStack)

testStackBranches :: TestTree
testStackBranches =
  testGroup
    "Stack Alternatives"
    [ testCase "Simple Stack Branch" $
        let Right (s, alt) =
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
        let Right (s, alt) = evalProgEntry8 0
         in (s, alt) @?= (S.fromList [i2SeUnsafe 101], S.empty),
      testCase "entry8 f1" $
        let Right (s, alt) = evalProgEntry8 1
         in (s, alt) @?= (S.fromList [i2SeUnsafe 103], S.empty),
      testCase "entry8 f2" $
        let Right (s, alt) = evalProgEntry8 2
         in (s, alt) @?= (S.fromList [i2SeUnsafe 106], S.empty),
      testCase "entry8 f3" $
        let Right (s, alt) = evalProgEntry8 3
         in (s, alt) @?= (S.fromList [i2SeUnsafe 110], S.empty),
      testCase "entry8 f4" $
        let Right (s, alt) = evalProgEntry8 4
         in (s, alt) @?= (S.fromList [i2SeUnsafe 115], S.empty),
      testCase "entry8 f5" $
        let Right (s, alt) = evalProgEntry8 5
         in (s, alt) @?= (S.fromList [i2SeUnsafe 121], S.empty),
      testCase "entry8 f6" $
        let Right (s, alt) = evalProgEntry8 6
         in (s, alt) @?= (S.fromList [i2SeUnsafe 128], S.empty),
      testCase "entry8 f7" $
        let Right (s, alt) = evalProgEntry8 7
         in (s, alt) @?= (S.fromList [i2SeUnsafe 136], S.empty)
    ]
  where
    evalProgEntry8 :: Integer -> Either ScriptError (VmStack, VmStack)
    evalProgEntry8 fIdx =
      evaluateProgWithStack
        progEntry8
        (startStack (succ fIdx) fIdx, S.empty)

    startStack :: Integer -> Integer -> VmStack
    startStack count fIdx =
      (i2SeUnsafe <$> S.fromList [1 .. count])
        |> i2SeUnsafe fIdx
        |> i2SeUnsafe 100

-- "choice" has to be positioned above the stack branch. Otherwise it can't be
-- fetched given that the stack branch depth is unknown.
progSimpleStackBranch ::
  FN
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
    # argRoll @"bytes"
    # opSize
    # op5
    # opEqualVerify
    # opDrop
    # argRoll @"choice"
    # op1
    # opEqual
    # opIf
      (branch1 # argDrop @"b1" # int 2)
      ( branch2
          # argDrop @"b2"
          # argDrop @"b3"
          # argRoll @"int"
          # int 29
          # opSub
      )

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
  FN
    ( s
        > (Args0 :| Args1 :| Args2 :| Args3 :| Args4 :| Args5 :| Args6 :| Args7)
        > N "_fIdx" TInt
        > Param
    )
    (s > TInt)
progEntry8 = entry8 f0 f1 f2 f3 f4 f5 f6 f7
  where
    f0 :: FN (Append s (Append Args0 '[Param])) (s > TInt)
    f0 =
      begin
        # (argRoll @"x0" # argRoll @"param")
        # opAdd
    f1 :: FN (Append s (Append Args1 '[Param])) (s > TInt)
    f1 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"param")
        # (opAdd # opAdd)
    f2 :: FN (Append s (Append Args2 '[Param])) (s > TInt)
    f2 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"x2" # argRoll @"param")
        # (opAdd # opAdd # opAdd)
    f3 :: FN (Append s (Append Args3 '[Param])) (s > TInt)
    f3 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"x2" # argRoll @"x3")
        # argRoll @"param"
        # (opAdd # opAdd # opAdd # opAdd)
    f4 :: FN (Append s (Append Args4 '[Param])) (s > TInt)
    f4 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"x2" # argRoll @"x3")
        # (argRoll @"x4" # argRoll @"param")
        # (opAdd # opAdd # opAdd # opAdd # opAdd)
    f5 :: FN (Append s (Append Args5 '[Param])) (s > TInt)
    f5 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"x2" # argRoll @"x3")
        # (argRoll @"x4" # argRoll @"x5" # argRoll @"param")
        # (opAdd # opAdd # opAdd # opAdd # opAdd # opAdd)
    f6 :: FN (Append s (Append Args6 '[Param])) (s > TInt)
    f6 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"x2" # argRoll @"x3")
        # (argRoll @"x4" # argRoll @"x5" # argRoll @"x6" # argRoll @"param")
        # (opAdd # opAdd # opAdd # opAdd # opAdd # opAdd # opAdd)
    f7 =
      begin
        # (argRoll @"x0" # argRoll @"x1" # argRoll @"x2" # argRoll @"x3")
        # (argRoll @"x4" # argRoll @"x5" # argRoll @"x6" # argRoll @"x7")
        # argRoll @"param"
        # (opAdd # opAdd # opAdd # opAdd # opAdd # opAdd # opAdd # opAdd)
