-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.TxIn (TxIn (..), OutPoint (..)) where

import Alba.Tx.Bch2025.SerializationUtils
  ( getLengthPrefixedByteString,
    putLengthPrefixedByteString,
  )
import Alba.Tx.Bch2025.TxId (TxId)
import Data.Binary (Binary (..))
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Prelude hiding (sequence)

data TxIn = TxIn
  { prevout :: OutPoint,
    scriptSig :: ByteString,
    sequence :: Word32
  }
  deriving (Eq, Show, Generic)

data OutPoint = OutPoint
  { txId :: TxId,
    index :: Word32
  }
  deriving (Eq, Show, Generic)

instance Binary TxIn where
  put (TxIn prevout scriptSig sequence) =
    put prevout >> putLengthPrefixedByteString scriptSig >> putWord32le sequence

  get = TxIn <$> get <*> getLengthPrefixedByteString <*> getWord32le

instance Binary OutPoint where
  put (OutPoint hash index) = put hash >> putWord32le index
  get = OutPoint <$> get <*> getWord32le
