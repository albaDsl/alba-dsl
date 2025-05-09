-- Copyright (c) 2025 albaDsl
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module TestIntPushing (testIntPushing) where

import Alba.Dsl.V1.Bch2025
  ( FN,
    Optimize (None),
    TInt,
    begin,
    compileL2,
    int,
    opAdd,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Common.ThUtils qualified as TH
import Alba.Vm.Bch2025
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Data.Sequence qualified as S
import QuickCheckSupport (VmInteger (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestUtils (evaluateProg, evaluateProgWithStack)

testIntPushing :: TestTree
testIntPushing =
  testGroup
    "Int pushing"
    [ testCase "Op0 - Op16" $
        let Right (s, alt) = evaluateProg progConstants
            c = compileL2 None progConstants
         in (s, alt, c)
              @?= ( S.singleton (i2SeUnsafe 136),
                    S.empty,
                    progConstantsCode
                  ),
      testCase "Push integers" $
        let Right (s, alt) = evaluateProg progPushInt
            c = compileL2 None progPushInt
         in (s, alt, c)
              @?= ( S.singleton (i2SeUnsafe 72057594037927681),
                    S.empty,
                    progPushIntCode
                  ),
      testProperty "Push arbitrary integers" propInt
    ]

progConstants :: FN s (s > TInt)
progConstants =
  begin
    # $(TH.foldrInts 'fn 'id [1 .. 16])
    # $(TH.foldrInts 'fnAdd 'id [1 .. 15])
  where
    fn n a = int n # a
    fnAdd _ a = opAdd # a

progConstantsCode :: CodeL2
progConstantsCode =
  S.fromList $
    [ OP_1,
      OP_2,
      OP_3,
      OP_4,
      OP_5,
      OP_6,
      OP_7,
      OP_8,
      OP_9,
      OP_10,
      OP_11,
      OP_12,
      OP_13,
      OP_14,
      OP_15,
      OP_16
    ]
      <> replicate 15 OP_ADD

progPushInt :: FN s (s > TInt)
progPushInt =
  begin
    # int 0x0100000000000002
    # int (-0x0102)
    # int 0
    # int 0x01
    # opAdd
    # opAdd
    # opAdd

progPushIntCode :: CodeL2
progPushIntCode =
  S.fromList
    [ OP_DATA L1.OP_DATA_08 [2, 0, 0, 0, 0, 0, 0, 1],
      OP_DATA L1.OP_DATA_02 [2, 129],
      OP_0,
      OP_1,
      OP_ADD,
      OP_ADD,
      OP_ADD
    ]

-- We manually scale down the integer by a few bytes using division, in order to
-- not run into SeScriptSize.
propInt :: VmInteger -> Bool
propInt (VmInteger x) =
  let stack = (S.empty, S.empty)
      x' = x `div` (256 ^ (3 :: Int))
   in case evaluateProgWithStack (int x') stack of
        Right (s, _) -> s == S.singleton (i2SeUnsafe x')
        Left err -> error $ show err
