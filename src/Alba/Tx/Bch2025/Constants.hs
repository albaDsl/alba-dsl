-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.Constants
  ( TokenBitfieldMask (..),
    prefixToken,
    minTokenAmount,
    maxTokenAmount,
    tokenBitfieldToWord8,
  )
where

import Data.Word (Word64, Word8)

prefixToken :: Word8
prefixToken = 0xef

minTokenAmount :: Word64
minTokenAmount = 1

maxTokenAmount :: Word64
maxTokenAmount = 9223372036854775807

data TokenBitfieldMask
  = HasCommitmentMask
  | HasNftMask
  | HasAmountMask
  | MintingMask
  | MutableMask

tokenBitfieldToWord8 :: TokenBitfieldMask -> Word8
tokenBitfieldToWord8 HasCommitmentMask = 0x40
tokenBitfieldToWord8 HasNftMask = 0x20
tokenBitfieldToWord8 HasAmountMask = 0x10
tokenBitfieldToWord8 MintingMask = 0x02
tokenBitfieldToWord8 MutableMask = 0x01
