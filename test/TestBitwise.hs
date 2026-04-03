-- Copyright (c) 2025 albaDsl

module TestBitwise (testBitwise) where

import Alba.Dsl.V1.Bch2026
import Alba.Misc.Utils (decodeHex)
import Alba.Vm.Common (b2SeUnsafe, i2SeUnsafe)
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import QuickCheckSupport (VmInteger (..), VmIntegerHalf (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import TestUtils2026
  ( evaluateProg,
    evaluateProgWithStack,
    getStack,
    isTrue,
    isTrue',
  )
import Prelude hiding (drop)

testBitwise :: TestTree
testBitwise =
  testGroup
    "Bitwise"
    [ testCase "Basic" $ isTrue (evaluateProg progBasic),
      testProperty "Double invert" propInvert,
      testProperty "ShiftBin: Left and back" propShiftBinLeftAndBack,
      testProperty "ShiftBin: Right and back" propShiftBinRightAndBack,
      testProperty "LShiftNum" propLShiftNum,
      testProperty "RShiftNum" propRShiftNum
    ]

progBasic :: Fn s (s > TBool)
progBasic =
  begin
    # (int 0b101 # nat 1 # opRShiftNum # int 0b10 # opEqualVerify)
    # (int 0b101 # nat 1 # opLShiftNum # int 0b1010 # opEqualVerify)
    # (int 0b101 # nat 10 # opRShiftNum # int 0 # opEqualVerify)
    # ( begin
          # (int 0b111111110000000010000011 # nat 1 # opRShiftNum)
          # (int 0b11111111000000001000001 # opEqualVerify)
      )
    # ( begin
          # (int (-0b11111111_00000000_10000011) # nat 1 # opRShiftNum)
          # (int (-0b1111111_10000000_01000010) # opEqualVerify)
      )
    # (int (-0b101) # nat 10 # opRShiftNum # int (-1) # opEqualVerify)
    # (int (-0b101) # nat 10 # opRShiftNum # int (-1) # opEqualVerify)
    # (bytes [0b0000_0101] # opInvert # bytes [0b1111_1010] # opEqualVerify)
    # ( begin
          # (bytes (fromJust $ decodeHex "deadbeef") # opInvert)
          # bytes (fromJust $ decodeHex "21524110")
          # opEqualVerify
      )
    # ( begin
          # (int 0 # nat 10_000 # opNum2Bin # opInvert # opSha256)
          # (bytes (fromJust $ decodeHex hash) # cast)
          # opEqualVerify
      )
    # ( begin
          # bytes [0b0000_0001, 0b1000_0000]
          # (nat 1 # opLShiftBin)
          # bytes [0b0000_0011, 0b0000_0000]
          # opEqualVerify
      )
    # ( begin
          # bytes [0b0000_0001, 0b1000_0000]
          # (nat 1 # opRShiftBin)
          # bytes [0b0000_0000, 0b1100_0000]
          # opEqualVerify
      )
    # ( begin
          # (bytes [0b0000_0001, 0b0000_0011] # cast)
          # (nat 1 # opRShiftNum # cast)
          # bytes [0b1000_0000, 0b0000_0001]
          # opEqualVerify
          # (bytes [0b0000_0001, 0b0000_0001] # cast)
          # (nat 1 # opRShiftNum # cast)
          # bytes [0b1000_0000, 0b0000_0000]
          # opEqualVerify
          # (bytes [0b1000_0000, 0b0000_0000] # cast)
          # (nat 1 # opRShiftNum # cast)
          # bytes [0b0100_0000]
          # opEqualVerify
          # (bytes [0b1000_0011, 0b0000_0000, 0b1111_1111] # cast)
          # (nat 1 # opRShiftNum # cast)
          # (bytes [0b0100_0010, 0b1000_0000, 0b1011_1111] # opEqualVerify)
      )
    # opTrue
  where
    hash = "2bf7158cb3d8f419f1e19ee71df61927cc17017f37ea8820f3bd719d2b4f88f8"

propInvert :: Bytes -> Bool
propInvert x =
  let stack = S.singleton $ b2SeUnsafe x
      stack' = getStack $ evaluateProgWithStack prog (stack, S.empty)
   in stack' == stack
  where
    prog :: Fn (s > TBytes) (s > TBytes)
    prog = opInvert # opInvert

-- Verifies that a stack element with the 'bitCount' left most bits cleared is
-- unchanged when bin shifted left and then back by 'bitCount' bits.
propShiftBinLeftAndBack :: Bytes -> Int -> Property
propShiftBinLeftAndBack x bitCount =
  (bitCount > 0 && bitCount <= B.length x * 8) ==>
    isTrue' $
      evaluateProgWithStack prog (S.singleton $ b2SeUnsafe x, S.empty)
  where
    prog :: Fn (s > TBytes) (s > TBool)
    prog =
      begin
        # (opDup # createMask # opAnd) -- clear leftmost bits.
        # (opDup # leftAndBack # opEqual)

    createMask :: Fn (s > TBytes) (s > TBytes)
    createMask = opDup # opInvert # opOr # leftAndBack

    leftAndBack :: Fn (s > TBytes) (s > TBytes)
    leftAndBack =
      begin
        # nat (fromIntegral bitCount)
        # opLShiftBin
        # nat (fromIntegral bitCount)
        # opRShiftBin

-- Verifies that a stack element with the 'bitCount' right most bits cleared is
-- unchanged when bin shifted right and then back by 'bitCount' bits.
propShiftBinRightAndBack :: Bytes -> Int -> Property
propShiftBinRightAndBack x bitCount =
  (bitCount > 0 && bitCount <= B.length x * 8) ==>
    isTrue' $
      evaluateProgWithStack prog (S.singleton $ b2SeUnsafe x, S.empty)
  where
    prog :: Fn (s > TBytes) (s > TBool)
    prog =
      begin
        # (opDup # createMask # opAnd) -- clear rightmost bits.
        # (opDup # rightAndBack # opEqual)

    createMask :: Fn (s > TBytes) (s > TBytes)
    createMask = opDup # opInvert # opOr # rightAndBack

    rightAndBack :: Fn (s > TBytes) (s > TBytes)
    rightAndBack =
      begin
        # nat (fromIntegral bitCount)
        # opRShiftBin
        # nat (fromIntegral bitCount)
        # opLShiftBin

-- For integers, a numeric left shift by one is equal to multiplication by 2.
propLShiftNum :: VmIntegerHalf -> Bool
propLShiftNum (VmIntegerHalf x) =
  isTrue' $ evaluateProgWithStack prog (S.singleton $ i2SeUnsafe x, S.empty)
  where
    prog :: Fn (s > TInt) (s > TBool)
    prog = opDup # int 2 # opMul # opSwap # nat 1 # opLShiftNum # opEqual

-- For positive integers, a numeric right shift by one is equal to division by
-- 2.
propRShiftNum :: VmInteger -> Property
propRShiftNum (VmInteger x) =
  (x >= 0) ==>
    isTrue' $
      evaluateProgWithStack prog (S.singleton $ i2SeUnsafe x, S.empty)
  where
    prog :: Fn (s > TInt) (s > TBool)
    prog = opDup # int 2 # opDiv # opSwap # nat 1 # opRShiftNum # opEqual
