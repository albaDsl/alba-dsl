-- Copyright (c) 2026 albaDsl

module Params (fee, deployAmount, contractAmount) where

import Data.Word (Word64)

-- The amount spent on the transaction that deploys the libraries and the
-- contract.
deployAmount :: Word64
deployAmount = 35_000

-- The amount that the contract itself will hold.
contractAmount :: Word64
contractAmount = 10_000

-- Fee for the spend transaction.
fee :: Word64
fee = 5000
