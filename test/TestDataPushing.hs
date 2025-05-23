-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestDataPushing (testDataPushing) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Bch2025 (vmParamsStandard)
import Alba.Vm.Common
  ( CodeL2,
    OpcodeL2 (OP_DATA, OP_NOP),
    ScriptError (SeScriptSize),
    VmParams (..),
    b2SeUnsafe,
  )
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Data.ByteString qualified as B
import Data.Sequence qualified as S
import QuickCheckSupport (BytesNonEmpty (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestUtils (evaluateProg, evaluateProgWithStack)

testDataPushing :: TestTree
testDataPushing =
  testGroup
    "Data pushing"
    [ testCase "Simple bytes push" $
        let res = evaluateProg progPush
            c = compileL2 None progPush
            c' = compile None progPush
         in (res, c, c')
              @?= ( Right
                      ( S.singleton $ b2SeUnsafe (B.pack [1, 2, 3]),
                        S.empty
                      ),
                    progPushCodeL2,
                    progPushCodeBin
                  ),
      testCase "Bytes push resulting in OP_PUSHDATA1" $
        let res = evaluateProg progPush250
            c = compileL2 None progPush250
            c' = compile None progPush250
         in (res, c, c')
              @?= ( Right
                      ( S.singleton $ b2SeUnsafe (B.replicate 250 0xff),
                        S.empty
                      ),
                    progPush250CodeL2,
                    progPush250CodeBin
                  ),
      testCase "Bytes max push" $
        let res = evaluateProg progPushLarge
            c = compileL2 None progPushLarge
            c' = compile None progPushLarge
         in (res, c, c')
              @?= ( Right
                      ( S.singleton $
                          b2SeUnsafe
                            (B.replicate largeElementSize 0xff),
                        S.empty
                      ),
                    progPushLargeCodeL2,
                    progPushLargeCodeBin
                  ),
      testCase "Bytes max push + 1" $
        let res = evaluateProg progPushMaxPlusOne
         in res @?= Left SeScriptSize,
      testProperty "Push arbitrary bytes" propBytes
    ]

-- We have a trailing opNop to ensure evaluator handles continued execution
-- after the data.
progPush :: FN s (s > TBytes)
progPush = bytes [1, 2, 3] # opNop

progPushCodeL2 :: CodeL2
progPushCodeL2 = S.fromList [OP_DATA L1.OP_DATA_03 [1, 2, 3], OP_NOP]

progPushCodeBin :: CodeL1
progPushCodeBin = B.pack [0x03, 0x01, 0x02, 0x03, 0x61]

progPush250 :: FN s (s > TBytes)
progPush250 = bytes (B.replicate 250 0xff) # opNop

progPush250CodeL2 :: CodeL2
progPush250CodeL2 =
  S.fromList [OP_DATA L1.OP_PUSHDATA1 (B.replicate 250 0xff), OP_NOP]

progPush250CodeBin :: CodeL1
progPush250CodeBin =
  B.pack [0x4c, 0xfa]
    <> B.replicate 250 0xff
    <> B.pack [0x61]

progPushLarge :: FN s (s > TBytes)
progPushLarge = bytes (B.replicate largeElementSize 0xff) # opNop

-- A little less than max to not run into SeScriptSize.
largeElementSize :: Int
largeElementSize = maxElementSize - 4

maxElementSize :: Int
maxElementSize = vmParamsStandard.maxScriptElementSize

progPushLargeCodeL2 :: CodeL2
progPushLargeCodeL2 =
  S.fromList
    [ OP_DATA L1.OP_PUSHDATA2 (B.replicate largeElementSize 0xff),
      OP_NOP
    ]

progPushLargeCodeBin :: CodeL1
progPushLargeCodeBin =
  B.pack [0x4d, 0x0c, 0x27]
    <> B.replicate largeElementSize 0xff
    <> B.pack [0x61]

-- Gives SeScriptSize rather than SePushSize.
progPushMaxPlusOne :: FN s (s > TBytes)
progPushMaxPlusOne =
  begin
    # bytes (B.replicate (maxElementSize + 1) 0xff)
    # opNop

-- We manually scale down the bytestring by a few bytes, in order to not run
-- into SeScriptSize.
propBytes :: BytesNonEmpty -> Bool
propBytes (BytesNonEmpty x) =
  let stack = (S.empty, S.empty)
      x' = if B.length x > 3 then B.drop 3 x else x
   in case evaluateProgWithStack (bytes x') stack of
        Right (s, _) -> s == S.singleton (b2SeUnsafe x')
        Left err -> error $ show err
