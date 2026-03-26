-- Copyright (c) 2026 albaDsl

module TestLzssBit (testLzssBit) where

import Alba.Dsl.V1.Bch2025 (Bytes, FN, TBool, bytes, opEqual, (#), type (>))
import Alba.Dsl.V1.Bch2026.Contract.LzssBit qualified as LZ
import Alba.Dsl.V1.Common.LzssBit (compress, decompress)
import Alba.Misc.Utils (decodeHex)
import Alba.Vm.Common (b2SeUnsafe, stackElementToBytes, stackTop)
import Data.ByteString qualified as B
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Word (Word8)
import QuickCheckSupport ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, withMaxSuccess, (==>))
import TestUtils (isTrue)
import TestUtils2026 (evaluateProg, evaluateProgWithStack, getStack)

testLzssBit :: TestTree
testLzssBit =
  testGroup
    "LZSS Bitstream Compression"
    [ testCase "LzssBit test vectors" $
        mapM_
          ( \(plain, compressed) -> do
              let plain' = fromMaybe err $ decodeHex plain
                  compressed' = fromMaybe err $ decodeHex compressed
              compress plain' @?= compressed'
              decompress compressed' @?= plain'
          )
          testVectors,
      testCase "LzssBit test vectors - VM decompressor" $
        mapM_
          ( \(plain, compressed) -> do
              let plain' = fromMaybe err $ decodeHex plain
                  compressed' = fromMaybe err $ decodeHex compressed
              isTrue $ evaluateProg (progDecompress plain' compressed')
          )
          testVectors,
      testProperty "LzssBit A/B sequences" propCompressDecompressAb,
      testProperty "LzssBit A/B sequences 2" propCompressDecompressAb2,
      testProperty "LzssBit sequences" $
        withMaxSuccess 10 propCompressDecompress,
      testProperty "LzssBit A/B sequences - CashVM" propCompressDecompressAbVm,
      testProperty "LzssBit sequences - CashVM" $
        withMaxSuccess 5 propCompressDecompressVm
    ]
  where
    err = error "err"

testVectors :: [(Text, Text)]
testVectors =
  [ ("41", "8300"),
    ("4142", "830A01"),
    ("414141", "83060D02"),
    ("414243414243", "830A1D020200"),
    ("4142414241424142", "830A990000"),
    ( "414243414243414243414243414243414243414243414243414243414243",
      "830A1DF202C02800"
    ),
    ( "4142434142434142434142434142434142434142434142434142434142434142434142"
        <> "43414243414243",
      "830A1DF202E029009800"
    )
  ]

progDecompress :: Bytes -> Bytes -> FN s (s > TBool)
progDecompress plain compressed =
  bytes compressed # LZ.decompress # bytes plain # opEqual

propCompressDecompressAb :: Bytes -> Property
propCompressDecompressAb str =
  (B.length (compress str') < B.length str') ==>
    case (decompress . compress) str' of
      s' | s' == str' -> True
      _ -> False
  where
    str' = B.map reduceCharSpace str

reduceCharSpace :: Word8 -> Word8
reduceCharSpace x
  | x < 128 = fromIntegral (ord 'A')
  | otherwise = fromIntegral (ord 'B')

-- Make sure we can compress strings larger than the window size.
propCompressDecompressAb2 :: Bytes -> Property
propCompressDecompressAb2 str =
  (B.length str > 4096 && B.length (compress str') < B.length str') ==>
    case (decompress . compress) str' of
      s' | s' == str' -> True
      _ -> False
  where
    str' = B.map reduceCharSpace str

propCompressDecompress :: Bytes -> Bool
propCompressDecompress str =
  case (decompress . compress) str of
    s' | s' == str -> True
    _ -> False

propCompressDecompressAbVm :: Bytes -> Property
propCompressDecompressAbVm str =
  (B.length (compress str') < B.length str') ==>
    (decompressVm . compress) str' == str'
  where
    str' = B.map reduceCharSpace str

decompressVm :: Bytes -> Bytes
decompressVm compressed =
  let stacks = (S.singleton $ b2SeUnsafe compressed, S.empty)
      s = getStack $ evaluateProgWithStack LZ.decompress stacks
   in maybe (error "err") stackElementToBytes (stackTop s)

-- If the string is random and does not compress, it will instead get larger by
-- one bit per byte. Thus we restrict the max length of the input bytestring
-- so that the compressed string fits in a stack entry.
propCompressDecompressVm :: Bytes -> Property
propCompressDecompressVm str =
  (B.length str < 8500) ==>
    (decompressVm . compress) str == str
