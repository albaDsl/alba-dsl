-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.TxContext
  ( TxContext,
    mkTxContext,
    txContextTx,
    txContextInputIndex,
    txContextCoins,
  )
where

import Alba.Vm.Common.Tx (Tx (..), TxOut (..))

data TxContext = TxContext
  { tx :: Tx,
    inputIndex :: Int,
    coins :: [TxOut]
  }
  deriving (Eq, Show)

mkTxContext :: Tx -> Int -> [TxOut] -> Maybe TxContext
mkTxContext tx inputIndex coins =
  if inputIndex < length tx.inputs && length tx.inputs == length coins
    then Just $ TxContext {..}
    else Nothing

txContextTx :: TxContext -> Tx
txContextTx TxContext {tx} = tx

txContextInputIndex :: TxContext -> Int
txContextInputIndex TxContext {inputIndex} = inputIndex

txContextCoins :: TxContext -> [TxOut]
txContextCoins TxContext {coins} = coins
