-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.TxId (TxId (..)) where

import Alba.Misc.Utils (decodeHex, encodeHex)
import Alba.Tx.Bch2025.Hash (Hash256 (..))
import Data.Binary (Binary (..), decode, encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (pack, unpack)
import GHC.Generics (Generic)

newtype TxId = TxId {id :: Hash256}
  deriving (Eq, Ord, Generic)

instance Show TxId where
  show = unpack . encodeHex . B.reverse . toStrict . encode

instance Binary TxId where
  put (TxId hash) = put hash
  get = TxId <$> get

instance IsString TxId where
  fromString str =
    let bs = fromMaybe (error "Failed to decode TxId") (decodeHex (pack str))
     in TxId . decode . fromStrict . B.reverse $ bs
