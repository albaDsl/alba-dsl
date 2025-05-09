-- Copyright (c) 2025 albaDsl

module Params (senderKeys, recipientKeys, timeoutStr, fundingTxAmount) where

import Alba.Misc.Haskoin (Network)
import Alba.Misc.KeyPair (KeyPair)
import Alba.Misc.Wallet (getWallet)
import Data.Word (Word64)

senderKeys :: Network -> IO (Maybe KeyPair)
senderKeys net = getWallet net "alice"

recipientKeys :: Network -> IO (Maybe KeyPair)
recipientKeys net = getWallet net "bob"

timeoutStr :: String
timeoutStr = "2025-04-27T11:30:00Z"

fundingTxAmount :: Word64
fundingTxAmount = 10_000
