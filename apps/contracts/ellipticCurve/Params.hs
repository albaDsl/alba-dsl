-- Copyright (c) 2026 albaDsl

module Params (fundingTxAmount, spendFee) where

import Data.Word (Word64)

fundingTxAmount :: Word64
fundingTxAmount = 20_000

spendFee :: Word64
spendFee = 15_000
