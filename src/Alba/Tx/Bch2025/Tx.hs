-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.Tx (Tx (..)) where

import Alba.Misc.Utils (decodeHex)
import Alba.Tx.Bch2025.SerializationUtils
  ( getLengthPrefixedArray,
    putLengthPrefixedArray,
  )
import Alba.Tx.Bch2025.TxIn (TxIn)
import Alba.Tx.Bch2025.TxOut (TxOut)
import Data.Binary (Binary (..), decode)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Generics (Generic)

data Tx = Tx
  { version :: Word32,
    inputs :: [TxIn],
    outputs :: [TxOut],
    lockTime :: Word32
  }
  deriving (Eq, Show, Generic)

instance Binary Tx where
  put tx = do
    putWord32le tx.version
    putLengthPrefixedArray tx.inputs
    putLengthPrefixedArray tx.outputs
    putWord32le tx.lockTime

  get =
    Tx
      <$> getWord32le
      <*> getLengthPrefixedArray
      <*> getLengthPrefixedArray
      <*> getWord32le

instance IsString Tx where
  fromString str =
    let bytes = (decodeHex . T.pack) str
     in decode (fromStrict (fromMaybe (error "Failed to decode tx.") bytes))
