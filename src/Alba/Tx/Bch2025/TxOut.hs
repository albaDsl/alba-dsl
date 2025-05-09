-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.TxOut
  ( TxOut (..),
    TokenData (..),
    NftCapability (..),
    Nft (..),
  )
where

import Alba.Misc.Haskoin (VarInt (VarInt), putVarInt)
import Alba.Tx.Bch2025.Constants
  ( TokenBitfieldMask (..),
    maxTokenAmount,
    minTokenAmount,
    prefixToken,
    tokenBitfieldToWord8,
  )
import Alba.Tx.Bch2025.Hash (Hash256 (..))
import Alba.Tx.Bch2025.SerializationUtils
  ( getLengthPrefixedByteString,
    putLengthPrefixedByteString,
  )
import Alba.Tx.Bch2025.TxId (TxId (..))
import Control.Monad (when)
import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.Binary.Get (Get, getWord64le)
import Data.Binary.Put (putWord64le)
import Data.Bits (zeroBits, (.&.), (.|.))
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.ByteString qualified as B
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

data TxOut = TxOut
  { value :: Word64,
    scriptPubKey :: ByteString,
    tokenData :: Maybe TokenData
  }
  deriving (Eq, Show, Generic)

data TokenData = TokenData
  { category :: TxId,
    amount :: Word64,
    nft :: Maybe Nft
  }
  deriving (Eq, Show, Generic)

data Nft = Nft
  { capability :: NftCapability,
    commitment :: ByteString
  }
  deriving (Eq, Show, Generic)

data NftCapability = None | Mutable | Minting
  deriving (Eq, Show, Generic)

instance Binary TxOut where
  put TxOut {..} = do
    putWord64le value
    case tokenData of
      Nothing -> putLengthPrefixedByteString scriptPubKey
      Just td -> do
        let tokenPrefix = encode td
        putLengthPrefixedByteString
          (B.singleton prefixToken <> toStrict tokenPrefix <> scriptPubKey)

  get = do
    value <- getWord64le
    bs <- getLengthPrefixedByteString
    (script, tokenData) <-
      if B.null bs || B.head bs /= prefixToken
        then pure (bs, Nothing)
        else case decodeOrFail (fromStrict $ B.tail bs) of
          Right (rest, _, tokenData') -> pure (toStrict rest, Just tokenData')
          Left (_, _, err) -> fail err
    pure $ TxOut value script tokenData

instance Binary TokenData where
  put td@TokenData {nft = Nothing, ..} = do
    put category
    put (calcTokenBitField td)
    when (amount > 0) $ putVarInt amount
  put td@TokenData {nft = Just Nft {..}, ..} = do
    put category
    put (calcTokenBitField td)
    when (commitment /= B.empty) $ putLengthPrefixedByteString commitment
    when (amount > 0) $ putVarInt amount

  get = do
    res <- get :: Get Hash256
    let category = TxId res
    tokenBitField <- get :: Get Word8
    commitment <-
      if tokenBitField .&. tb HasCommitmentMask /= 0
        then getLengthPrefixedByteString
        else pure B.empty
    amount <-
      if tokenBitField .&. tb HasAmountMask /= 0
        then do
          VarInt x <- get
          if x >= minTokenAmount && x <= maxTokenAmount
            then pure x
            else fail "TokenData: amount out of range."
        else pure 0
    pure $
      if tokenBitField .&. tb HasNftMask /= 0
        then
          TokenData
            { nft = Just Nft {capability = calcCap tokenBitField, ..},
              ..
            }
        else TokenData {nft = Nothing, ..}
    where
      tb = tokenBitfieldToWord8

      calcCap :: Word8 -> NftCapability
      calcCap tokenBitField
        | tokenBitField .&. tb MutableMask /= 0 = Mutable
        | tokenBitField .&. tb MintingMask /= 0 = Minting
        | otherwise = None

calcTokenBitField :: TokenData -> Word8
calcTokenBitField TokenData {..} =
  let hasAmount = amount /= 0
   in case nft of
        Nothing -> maybeSet (tb HasAmountMask) hasAmount zeroBits
        Just Nft {..} ->
          let hasNft = True
              hasCommitment = commitment /= B.empty
           in ( maybeSet (tb HasCommitmentMask) hasCommitment
                  . maybeSet (tb HasNftMask) hasNft
                  . maybeSet (tb HasAmountMask) hasAmount
                  . maybeSet (tb MintingMask) (capability == Minting)
                  . maybeSet (tb MutableMask) (capability == Mutable)
              )
                zeroBits
  where
    tb = tokenBitfieldToWord8

    maybeSet :: Word8 -> Bool -> Word8 -> Word8
    maybeSet _bit False input = input
    maybeSet bit True input = input .|. bit
