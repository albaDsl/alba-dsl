-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.SerializationUtils
  ( getLengthPrefixedByteString,
    putLengthPrefixedByteString,
    getLengthPrefixedArray,
    putLengthPrefixedArray,
  )
where

import Alba.Misc.Haskoin (VarInt (VarInt), putVarInt)
import Control.Monad (replicateM)
import Data.Binary (Binary (..))
import Data.Binary.Get (Get, getByteString)
import Data.Binary.Put (Put, putByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B

getLengthPrefixedByteString :: Get ByteString
getLengthPrefixedByteString = do
  VarInt len <- get
  getByteString (fromIntegral len)

putLengthPrefixedByteString :: ByteString -> Put
putLengthPrefixedByteString bs = do
  putVarInt $ B.length bs
  putByteString bs

getLengthPrefixedArray :: (Binary a) => Get [a]
getLengthPrefixedArray = do
  (VarInt count) <- get
  replicateM (fromIntegral count) get

putLengthPrefixedArray :: (Binary a) => [a] -> Put
putLengthPrefixedArray xs = do
  putVarInt $ length xs
  mapM_ put xs
