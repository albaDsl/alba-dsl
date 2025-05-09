-- Copyright (c) 2025 albaDsl

module Alba.Misc.MockVals (mockTxId, mockAddr) where

import Alba.Misc.Haskoin (Address (..), mainnet, textToAddr)
import Alba.Tx.Bch2025 (TxId)
import Data.Maybe (fromJust)

mockTxId :: TxId
mockTxId = "89cf399ffae56ae5d20fba213049f22ce1ed669c1b862868d9415ad3d208b325"

mockAddr :: Address
mockAddr =
  fromJust $
    textToAddr
      mainnet
      "bitcoincash:qzakufxk820c0vu0yzfwpth209kunvrl9cddemueaf"
