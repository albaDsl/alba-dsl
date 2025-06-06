-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.VmInteger
  ( bytesToInteger,
    integerToBytes,
    integerByteSize,
    extendToByteSize,
    maxInteger,
  )
where

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.BasicTypes (Bytes)
import Data.Bits (clearBit, complement, shift, shiftR, testBit, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import GHC.Num (integerLog2)
import Prelude hiding (init, iterate, sum)

signBit :: Int
signBit = 7

signBitMask :: Word8
signBitMask = 0x80

allBitsMask :: Word8
allBitsMask = 0xff

bitsPerByte :: Int
bitsPerByte = 8

bytesToInteger :: Bytes -> Integer
bytesToInteger bytes | B.length bytes == 0 = 0
bytesToInteger bytes =
  let msb = B.last bytes
      n = B.foldr f 0 (B.init bytes <> B.singleton (clearBit msb signBit))
   in if testBit msb signBit then -n else n
  where
    f :: Word8 -> Integer -> Integer
    f byte n = n `shift` bitsPerByte .|. fromIntegral byte

integerToBytes :: Integer -> Bytes
integerToBytes 0 = B.empty
integerToBytes n =
  let neg = n < 0
      res = B.unfoldr f (abs n)
      (init, msb) = fromMaybe canNotHappen (B.unsnoc res)
   in if testBit msb signBit
        then B.snoc res (if neg then signBitMask else 0)
        else if neg then B.snoc init (msb .|. signBitMask) else res
  where
    f :: Integer -> Maybe (Word8, Integer)
    f 0 = Nothing
    f x = Just (fromIntegral x, x `shiftR` bitsPerByte)

{-# INLINE integerByteSize #-}
integerByteSize :: Integer -> Int
integerByteSize 0 = 0
integerByteSize n =
  let n' = abs n
      numBits = integerLog2 n'
      (q, r) = numBits `quotRem` 8
      topBitSet = r == 7
      extraByte = if topBitSet then 1 else 0
   in fromIntegral $ q + 1 + extraByte

extendToByteSize :: Bytes -> Int -> Maybe Bytes
extendToByteSize bytes byteSize =
  let bytes' = minimallyEncodeInteger bytes
   in extendInteger bytes' byteSize

-- FIXME performance.
minimallyEncodeInteger :: Bytes -> Bytes
minimallyEncodeInteger = integerToBytes . bytesToInteger

extendInteger :: Bytes -> Int -> Maybe Bytes
extendInteger x desiredSize | B.length x > desiredSize = Nothing
extendInteger x desiredSize | B.length x == desiredSize = Just x
extendInteger x desiredSize =
  let currentSize = B.length x
      (signBit', x') =
        if currentSize > 0
          then
            let (init, msb) = fromMaybe canNotHappen (B.unsnoc x)
             in ( msb .&. signBitMask,
                  B.snoc init (msb .&. complement signBitMask)
                )
          else
            (0, x)
   in Just $
        x'
          <> B.replicate (desiredSize - currentSize - 1) 0
          <> B.singleton signBit'

maxInteger :: Int -> Bool -> Integer
maxInteger size negative =
  let msb = if negative then allBitsMask else complement signBitMask
   in bytesToInteger (B.replicate (size - 1) allBitsMask <> [msb])
