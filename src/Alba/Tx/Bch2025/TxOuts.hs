-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.TxOuts (TxOuts (..)) where

import Alba.Tx.Bch2025.SerializationUtils (getLengthPrefixedArray)
import Alba.Tx.Bch2025.TxOut (TxOut)
import Data.Binary (Binary (..))

newtype TxOuts = TxOuts {get :: [TxOut]}
  deriving (Show)

instance Binary TxOuts where
  get = TxOuts <$> getLengthPrefixedArray
  put _x = error "Not implemented."
