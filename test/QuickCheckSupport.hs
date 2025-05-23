-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-orphans #-}

module QuickCheckSupport where

import Alba.Vm.Bch2025 (vmParamsStandard)
import Alba.Vm.Common
  ( Bytes,
    HugeInt (..),
    VmParams (integerMax, integerMin, maxScriptElementSize),
  )
import Data.ByteString qualified as B
import Data.Word (Word8)
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    chooseInteger,
    listOf,
    listOf1,
    resize,
    vectorOf,
  )

bytesMaxSize :: Int
bytesMaxSize = vmParamsStandard.maxScriptElementSize

bytesHalfMaxSize :: Int
bytesHalfMaxSize = bytesMaxSize `div` 2

integerMin :: Integer
integerMin = vmParamsStandard.integerMin.val

integerMax :: Integer
integerMax = vmParamsStandard.integerMax.val

newtype BytesNonEmpty = BytesNonEmpty Bytes
  deriving (Show)

newtype BytesHalf = BytesHalf Bytes
  deriving (Show)

newtype VmInteger = VmInteger Integer
  deriving (Show)

newtype LargeInteger = LargeInteger Integer
  deriving (Show)

instance Arbitrary Bytes where
  arbitrary = do
    x <- resize bytesMaxSize (listOf byte)
    pure $ B.pack x

instance Arbitrary BytesNonEmpty where
  arbitrary = do
    x <- resize bytesMaxSize (listOf1 byte)
    pure $ BytesNonEmpty (B.pack x)

instance Arbitrary BytesHalf where
  arbitrary = do
    x <- resize bytesHalfMaxSize (listOf byte)
    pure $ BytesHalf (B.pack x)

byte :: Gen Word8
byte = arbitrary

instance Arbitrary VmInteger where
  arbitrary = do
    x <- chooseInteger (integerMin, integerMax) :: Gen Integer
    pure $ VmInteger x

genByteStringOfSize :: Int -> Gen B.ByteString
genByteStringOfSize n = do
  x <- vectorOf n (choose (0, 255) :: Gen Word8)
  pure $ B.pack x
