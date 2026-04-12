-- Copyright (c) 2026 albaDsl

module TestLzss (testLzss) where

import Alba.Dsl.V1.Bch2025 (Bytes, Fn, TBool, bytes, opEqual, (∘), type (>))
import Alba.Dsl.V1.Bch2026.Contract.Lzss qualified as LZ
import Alba.Dsl.V1.Common.Lzss (compress, decompress)
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
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, withMaxSuccess, (==>))
import TestUtils (isTrue)
import TestUtils2026 (evaluateProg, evaluateProgWithStack, getStack)

testLzss :: TestTree
testLzss =
  testGroup
    "LZSS Compression"
    [ testCase "LZSS test vectors" $
        mapM_
          ( \(plain, compressed) -> do
              let plain' = fromMaybe err $ decodeHex plain
                  compressed' = fromMaybe err $ decodeHex compressed
              compress plain' @?= compressed'
              case decompress compressed' of
                Right val -> val @?= plain'
                Left _ -> assertFailure "test vectors"
          )
          testVectors,
      testCase "LZSS test vectors - VM decompressor" $
        mapM_
          ( \(plain, compressed) -> do
              let plain' = fromMaybe err $ decodeHex plain
                  compressed' = fromMaybe err $ decodeHex compressed
              isTrue $ evaluateProg (progDecompress plain' compressed')
          )
          testVectors,
      testProperty "LZSS A/B sequences" propCompressDecompressAb,
      testProperty "LZSS A/B sequences 2" propCompressDecompressAb2,
      testProperty "LZSS sequences" propCompressDecompress,
      testProperty "LZSS A/B sequences - CashVM" propCompressDecompressAbVm,
      testProperty "LZSS sequences - CashVM" $
        withMaxSuccess 20 propCompressDecompressVm
    ]
  where
    err = error "err"

testVectors :: [(Text, Text)]
testVectors =
  [ ("41", "0141"),
    ("4142", "034142"),
    ("414141", "07414141"),
    ("414243414243", "074142432000"),
    ("4142414241424142", "0341421300"),
    ( "414243414243414243414243414243414243414243414243414243414243",
      "074142432f004601"
    ),
    ( "4142434142434142434142434142434142434142434142434142434142434142434142"
        <> "43414243414243",
      "074142432f004f016002"
    )
  ]

progDecompress :: Bytes -> Bytes -> Fn s (s > TBool)
progDecompress plain compressed =
  bytes compressed ∘ LZ.decompress ∘ bytes plain ∘ opEqual

propCompressDecompressAb :: Bytes -> Property
propCompressDecompressAb str =
  (B.length (compress str') < B.length str') ==>
    case (decompress . compress) str' of
      Right s' | s' == str' -> True
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
      Right s' | s' == str' -> True
      _ -> False
  where
    str' = B.map reduceCharSpace str

propCompressDecompress :: Bytes -> Bool
propCompressDecompress str =
  case (decompress . compress) str of
    Right s' | s' == str -> True
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
