-- Copyright (c) 2026 albaDsl
{-# LANGUAGE Strict #-}

module Alba.Dsl.V1.Common.Lzss (compress, decompress) where

import Alba.Dsl.V1.Common.LzssCommon
  ( Token (..),
    bitsPerByte,
    copyFromBack,
    groupSize,
    lenBias,
    lenBits,
    lenMask,
    offBias,
    refLen,
    refToVal,
    tokens,
  )
import Data.Bits (Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)

compress :: ByteString -> ByteString
compress bs = B.pack (emit (tokens bs))
  where
    emit :: [Token] -> [Word8]
    emit [] = []
    emit ts =
      let (group, rest) = splitAt groupSize ts
          flag = buildFlag group
          bytes = concatMap emitToken group
       in flag : bytes ++ emit rest

    buildFlag :: [Token] -> Word8
    buildFlag = go 0
      where
        go _ [] = 0
        go k (Lit _ : xs) = setBit (go (k + 1) xs) k
        go k (Ref _ _ : xs) = go (k + 1) xs

    emitToken :: Token -> [Word8]
    emitToken (Lit b) = [b]
    emitToken ref =
      let ref' = refToVal ref
          b1 = fromIntegral (ref' .&. 0xFF)
          b2 = fromIntegral (ref' `shiftR` bitsPerByte)
       in [b1, b2]

decompress :: ByteString -> Either String ByteString
decompress bs = loop 0 8 0 B.empty
  where
    n = B.length bs

    loop i k flag out
      | i >= n = Right out
      | k == 8 = loop (i + 1) 0 (B.index bs i) out
      | testBit flag k =
          let b = B.index bs i
              out' = B.snoc out b
           in loop (i + 1) (k + 1) flag out'
      | otherwise =
          if i + 1 >= n
            then Left "Unexpected end of input in reference"
            else
              let b1 = fromIntegral (B.index bs i) :: Int
                  b2 = fromIntegral (B.index bs (i + 1)) :: Int
                  v = (b2 `shiftL` bitsPerByte) .|. b1
                  off = (v `shiftR` lenBits) + offBias
                  len = (v .&. lenMask) + lenBias
                  out' = copyFromBack out off len
               in loop (i + refLen) (k + 1) flag out'
