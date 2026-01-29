-- Copyright (c) 2026 albaDsl
{-# LANGUAGE Strict #-}

module Alba.Dsl.V1.Common.Lzss (compress, decompress) where

import Data.Bits (Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)

data Token
  = Lit Word8
  | Ref Int Int
  deriving (Eq, Show)

bitsPerByte :: Int
bitsPerByte = 8

winLen :: Int
winLen = 4096

groupSize :: Int
groupSize = 8

minMatchLen :: Int
minMatchLen = 3

maxMatchLen :: Int
maxMatchLen = 18

-- Number of bits used to store the match length in the reference.
lengthBits :: Int
lengthBits = 4

-- Size in bytes of a reference.
refLen :: Int
refLen = 2

-- ## Indices used in compress/decompress below.
-- i: index into the ByteString to compress/decompress.
-- p: index into the ByteString to compress. 'p' is restricted to be inside the
-- sliding window.
-- k: index into a group of eight tokens.

compress :: ByteString -> ByteString
compress bs = B.pack (emit (tokens 0))
  where
    n = B.length bs

    tokens :: Int -> [Token]
    tokens i
      | i >= n = []
      | len >= minMatchLen = Ref off len : tokens (i + len)
      | otherwise = Lit (B.index bs i) : tokens (i + 1)
      where
        (off, len) = bestMatch i

    bestMatch :: Int -> (Int, Int)
    bestMatch i = go windowStart (0, 0)
      where
        windowStart = max 0 (i - winLen)
        maxCandidateLength = min maxMatchLen (n - i)

        go p best
          | p >= i = best
          | otherwise =
              let l = matchLen i p maxCandidateLength
                  best' = if l > snd best then (i - p, l) else best
               in go (p + 1) best'

    matchLen :: Int -> Int -> Int -> Int
    matchLen i p maxLen = loop 0
      where
        loop len
          | len >= maxLen = len
          | B.index bs (i + len) /= B.index bs (p + len) = len
          | otherwise = loop (len + 1)

    emit :: [Token] -> [Word8]
    emit [] = []
    emit ts =
      let (group, rest) = splitAt groupSize ts
          flag = buildFlag group
          bytes = concatMap emitTok group
       in flag : bytes ++ emit rest

    buildFlag :: [Token] -> Word8
    buildFlag = go 0
      where
        go _ [] = 0
        go k (Lit _ : xs) = setBit (go (k + 1) xs) k
        go k (Ref _ _ : xs) = go (k + 1) xs

    emitTok :: Token -> [Word8]
    emitTok (Lit b) = [b]
    emitTok (Ref off len) =
      let ref = ((off - 1) `shiftL` lengthBits) .|. (len - minMatchLen)
          b1 = fromIntegral (ref .&. 0xFF)
          b2 = fromIntegral (ref `shiftR` bitsPerByte)
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
                  off = (v `shiftR` lengthBits) + 1
                  len = (v .&. 0xF) + minMatchLen
                  out' = copyFromBack out off len
               in loop (i + refLen) (k + 1) flag out'

    copyFromBack :: ByteString -> Int -> Int -> ByteString
    copyFromBack out off len = step 0 out
      where
        start = B.length out - off

        step j acc
          | j >= len = acc
          | otherwise =
              let b = B.index acc (start + j)
               in step (j + 1) (B.snoc acc b)
