-- Copyright (c) 2025 albaDsl

module Alba.Node.ValidateTokens (verifyTxTokens) where

import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025
  ( Nft (..),
    NftCapability (..),
    TokenData (..),
    Tx (..),
    TxId (..),
    TxOut (..),
  )
import Alba.Tx.Bch2025.TxIn (OutPoint (..), TxIn (..))
import Alba.Vm.Common.TxContext (TxContext, txContextCoins, txContextTx)
import Control.Applicative ((<|>))
import Control.Monad (unless, void)
import Data.ByteString qualified as B
import Data.Foldable (foldrM)
import Data.Map (Map, empty, insert, insertWith, lookup, singleton, unionWith)
import Data.Word (Word64)
import Prelude hiding (lookup)

data TokenState = TokenState
  { amounts :: !(Map TxId Integer),
    minting :: !(Map TxId ()),
    mutables :: !(Map TxId Word64),
    immutables :: !(Map TxId (Map B.ByteString Word64)),
    genesis :: !(Map TxId Integer)
  }
  deriving (Show)

verifyTxTokens :: TxContext -> Either ValidationFailure ()
verifyTxTokens txContext = do
  let utxos = txContextCoins txContext
      tx = txContextTx txContext
      combined = zip utxos tx.inputs
  st <- foldrM verifyInputs startTokenState combined
  void $ foldrM verifyOutputs st tx.outputs
  where
    startTokenState :: TokenState
    startTokenState =
      TokenState
        { amounts = empty,
          minting = empty,
          mutables = empty,
          immutables = empty,
          genesis = empty
        }

verifyInputs ::
  (TxOut, TxIn) ->
  TokenState ->
  Either ValidationFailure TokenState
verifyInputs (TxOut {tokenData = Nothing}, txIn) st =
  Right $ genesisHash txIn.prevout st
verifyInputs
  (TxOut {tokenData = Just TokenData {..}}, txIn)
  st@TokenState {..} = do
    let st' =
          genesisHash txIn.prevout $
            st {amounts = insertWith (+) category (fromIntegral amount) amounts}
    case nft of
      Just Nft {..} -> do
        verifyCommitmentSize commitment
        case capability of
          None -> do
            let m = singleton commitment 1
                union = unionWith (+)
            Right $ st' {immutables = insertWith union category m immutables}
          Mutable -> Right $ st' {mutables = insertWith (+) category 1 mutables}
          Minting -> Right $ st' {minting = insert category () minting}
      Nothing -> Right st'

genesisHash :: OutPoint -> TokenState -> TokenState
genesisHash op st | op.index == 0 = st {genesis = insert op.txId 0 st.genesis}
genesisHash _ st = st

verifyCommitmentSize :: B.ByteString -> Either ValidationFailure ()
verifyCommitmentSize commitment =
  unless (B.length commitment <= maxCommitment) $ Left VfCommitmentOversize
  where
    maxCommitment = 40 :: Int

verifyOutputs :: TxOut -> TokenState -> Either ValidationFailure TokenState
verifyOutputs (TxOut {tokenData = Nothing}) st = Right st
verifyOutputs (TxOut {tokenData = Just TokenData {..}}) st@TokenState {..} =
  do
    (st', isGenesis) <-
      case (lookup category amounts, lookup category genesis) of
        (Just inputsAmount, Nothing) -> do
          let x = inputsAmount - fromIntegral amount
          if x >= 0
            then pure (st {amounts = insert category x amounts}, False)
            else Left VfTokenOverSpend
        (Nothing, Just genesisAmount) -> do
          let x = genesisAmount + fromIntegral amount
          if x <= maxFungibleTokens
            then pure (st {genesis = insert category x genesis}, True)
            else Left VfTokensOverflow
        _ -> Left VfInvalidCategory
    case nft of
      Just nft'@Nft {commitment} -> do
        verifyCommitmentSize commitment
        if isGenesis
          then Right st'
          else case spendImmutable nft' st'
            <|> spendMutable nft' st'
            <|> spendMinting st' of
            Just st'' -> Right st''
            Nothing -> Left VfNftExNihilo
      _ -> Right st'
  where
    spendImmutable Nft {..} state | capability == None = do
      m <- lookup category state.immutables
      c <- lookup commitment m
      let m' = insert commitment (pred c) m
          state' = state {immutables = insert category m' state.immutables}
      if c > 0 then Just state' else Nothing
    spendImmutable _ _ = Nothing

    spendMutable Nft {..} state | capability /= Minting = do
      c <- lookup category state.mutables
      let state' = state {mutables = insert category (pred c) state.mutables}
      if c > 0 then Just state' else Nothing
    spendMutable _ _ = Nothing

    spendMinting state = lookup category state.minting >> Just state

    maxFungibleTokens = 9223372036854775807 :: Integer
