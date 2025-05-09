-- Copyright (c) 2025 albaDsl

module Params
  ( refreshKeys,
    withdrawKeys,
    inheritKeys,
    refreshDelay,
    inheritDelay,
    fundingTxAmount,
    fee,
  )
where

import Alba.Dsl.V1.Bch2025 (days)
import Alba.Misc.Haskoin (Network)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Wallet (getWallet)
import Data.Word (Word64)
import Numeric.Natural (Natural)

refreshKeys :: Network -> IO (Maybe KeyPair)
refreshKeys net = getWallet net "alice"

withdrawKeys :: Network -> IO (Maybe KeyPair)
withdrawKeys net = getWallet net "alice-2"

inheritKeys :: Network -> IO (Maybe KeyPair)
inheritKeys net = getWallet net "bob"

refreshDelay :: Natural
refreshDelay = days 7

inheritDelay :: Natural
inheritDelay = days 180

fundingTxAmount :: Word64
fundingTxAmount = 10_000

fee :: Word64
fee = 1000
