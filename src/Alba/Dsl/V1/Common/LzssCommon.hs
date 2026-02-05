-- Copyright (c) 2026 albaDsl
{-# LANGUAGE Strict #-}

module Alba.Dsl.V1.Common.LzssCommon where

import Data.Bits (Bits (shiftL, (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)

refLen, refBits, offBits, offBias, lenBits, lenBias, lenMask :: Int
refLen = 2 -- Size in bytes of a reference.
refBits = offBits + lenBits -- Size in bits of a reference.
offBits = 12 -- Number of bits used to store the offset in the reference.
offBias = 1 -- Bias to add to the offset field.
lenBits = 4 -- Number of bits used to store the match length in the reference.
lenBias = 3 -- Bias to add to the length field.
lenMask = (1 `shiftL` lenBits) - 1

groupSize, winLen, minMatch, maxMatch, bitsPerByte :: Int
groupSize = 8
winLen = 1 `shiftL` offBits -- Size in bytes of sliding window.
minMatch = 3
maxMatch = minMatch + (1 `shiftL` lenBits) - 1
bitsPerByte = 8

data Token
  = Lit Word8
  | Ref Int Int
  deriving (Eq, Show)

tokens :: ByteString -> [Token]
tokens bs = tokens' bs 0

tokens' :: ByteString -> Int -> [Token]
tokens' bs i
  | i >= n = []
  | len >= minMatch = Ref off len : tokens' bs (i + len)
  | otherwise = Lit (B.index bs i) : tokens' bs (i + 1)
  where
    n = B.length bs
    (off, len) = bestMatch bs i

bestMatch :: ByteString -> Int -> (Int, Int)
bestMatch bs i = go windowStart (0, 0)
  where
    n = B.length bs
    windowStart = max 0 (i - winLen)
    maxCandidateLength = min maxMatch (n - i)

    go p best
      | p >= i = best
      | otherwise =
          let l = matchLen bs i p maxCandidateLength
              best' = if l > snd best then (i - p, l) else best
           in go (p + 1) best'

matchLen :: ByteString -> Int -> Int -> Int -> Int
matchLen bs i p maxLen = loop 0
  where
    loop len
      | len >= maxLen = len
      | B.index bs (i + len) /= B.index bs (p + len) = len
      | otherwise = loop (len + 1)

copyFromBack :: ByteString -> Int -> Int -> ByteString
copyFromBack out off len = step 0 out
  where
    start = B.length out - off

    step j acc
      | j >= len = acc
      | otherwise =
          let b = B.index acc (start + j)
           in step (j + 1) (B.snoc acc b)

refToVal :: Token -> Int
refToVal (Ref off len) = ((off - offBias) `shiftL` lenBits) .|. (len - lenBias)
refToVal _ = error "Not a reference."
