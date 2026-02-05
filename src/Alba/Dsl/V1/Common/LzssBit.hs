-- Copyright (c) 2026 albaDsl
{-# LANGUAGE Strict #-}

-- A packed bitstream version of Lzss.

module Alba.Dsl.V1.Common.LzssBit (compress, decompress) where

import Alba.Dsl.V1.Common.LzssCommon
  ( Token (..),
    bitsPerByte,
    copyFromBack,
    lenBias,
    lenBits,
    lenMask,
    offBias,
    refBits,
    refToVal,
    tokens,
  )
import Data.Bits (Bits (setBit, shiftR, testBit, (.&.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

compress :: ByteString -> ByteString
compress bs = bitsToBytes (emit (tokens bs) [])
  where
    emit :: [Token] -> [Bool] -> [Bool]
    emit [] = id
    emit (t : ts) = emitToken t . emit ts

    emitToken :: Token -> [Bool] -> [Bool]
    emitToken (Lit byte) = emitBit True . emitLiteral byte
    emitToken ref = emitBit False . emitRef (refToVal ref)

    emitBit :: Bool -> [Bool] -> [Bool]
    emitBit bit = (bit :)

    emitLiteral :: Word8 -> [Bool] -> [Bool]
    emitLiteral byte = (byteToBits byte ++)

    emitRef :: Int -> [Bool] -> [Bool]
    emitRef ref = (refToBits ref ++)

    bitsToBytes :: [Bool] -> ByteString
    bitsToBytes bits = B.pack (go bits)
      where
        go [] = []
        go xs =
          let (chunk, rest) = splitAt bitsPerByte xs
              padded = chunk ++ replicate (bitsPerByte - length chunk) False
           in byteFromBits padded : go rest

decompress :: ByteString -> ByteString
decompress bs = go (bytesToBits bs) B.empty
  where
    go :: [Bool] -> ByteString -> ByteString
    go bits acc =
      fromMaybe
        acc
        ( do
            (flag, rest) <- getBit bits
            if flag
              then do
                (bits', rest') <- getBits bitsPerByte rest
                pure $ go rest' (B.snoc acc (byteFromBits bits'))
              else do
                (bits', rest') <- getBits refBits rest
                let ref = refFromBits bits'
                    off = (ref `shiftR` lenBits) + offBias
                    len = (ref .&. lenMask) + lenBias
                pure $ go rest' (copyFromBack acc off len)
        )

    bytesToBits :: ByteString -> [Bool]
    bytesToBits = concatMap byteToBits . B.unpack

    getBit :: [Bool] -> Maybe (Bool, [Bool])
    getBit [] = Nothing
    getBit (bit : bits) = Just (bit, bits)

    getBits :: Int -> [Bool] -> Maybe ([Bool], [Bool])
    getBits k bits
      | length bits < k = Nothing
      | otherwise = pure $ splitAt k bits

byteToBits :: Word8 -> [Bool]
byteToBits w = [testBit w i | i <- [0 .. bitsPerByte - 1]]

byteFromBits :: [Bool] -> Word8
byteFromBits = fromIntegral . bitsToIntVal

refToBits :: Int -> [Bool]
refToBits v = [testBit v i | i <- [0 .. refBits - 1]]

refFromBits :: [Bool] -> Int
refFromBits = bitsToIntVal

bitsToIntVal :: [Bool] -> Int
bitsToIntVal = foldl set 0 . zip [0 ..]
  where
    set acc (i, True) = setBit acc i
    set acc (_, False) = acc
