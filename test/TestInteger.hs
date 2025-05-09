-- Copyright (c) 2025 albaDsl

module TestInteger (testInteger) where

import Alba.Vm.Bch2025.VmParams qualified as VP
import Alba.Vm.Common
import Data.ByteString qualified as B
import QuickCheckSupport (VmInteger (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

testInteger :: TestTree
testInteger =
  testGroup
    "Integers"
    [ testProperty "Integer byte conversion" propEncodeDecode,
      testProperty "Integer byte size calculation" propByteSize,
      testCase "Integer min/max" $
        ( B.length $ integerToBytes VP.vmParamsStandard.integerMax.val,
          B.length $ integerToBytes VP.vmParamsStandard.integerMin.val
        )
          @?= ( VP.vmParamsStandard.maxIntegerBytes,
                VP.vmParamsStandard.maxIntegerBytes
              )
    ]

propEncodeDecode :: VmInteger -> Bool
propEncodeDecode (VmInteger x) = (bytesToInteger . integerToBytes) x == x

propByteSize :: VmInteger -> Bool
propByteSize (VmInteger x) = B.length (integerToBytes x) == integerByteSize x
