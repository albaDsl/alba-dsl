-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.Common where

import Data.Bits (Bits (shiftL, shiftR))

countTrailingZeros :: Integer -> Int
countTrailingZeros 0 = 0
countTrailingZeros n = go 0 n
  where
    go !k m
      | even m = go (k + 1) (m `shiftR` 1)
      | otherwise = k

mods :: Integer -> Int -> Integer
mods n modulo =
  let full = 1 `shiftL` modulo
      half = full `shiftR` 1
      r = n `mod` full
   in if r >= half then r - full else r
