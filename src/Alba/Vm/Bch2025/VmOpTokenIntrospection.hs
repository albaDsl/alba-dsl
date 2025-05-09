-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpTokenIntrospection (evalOpTokenIntrospection) where

import Alba.Tx.Bch2025
  ( Hash256 (..),
    Nft (..),
    NftCapability (..),
    TokenData (..),
    Tx (..),
    TxId (..),
    TxOut (..),
  )
import Alba.Vm.Bch2025.TxContext (TxContext, txContextCoins, txContextTx)
import Alba.Vm.Bch2025.Utils (indexCheck, op1)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (Bytes, StackElement)
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word64)

{- ORMOLU_DISABLE -}
evalOpTokenIntrospection ::
  OpcodeL2 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpTokenIntrospection op txContext st =
  case op of
    OP_UTXOTOKENCATEGORY ->     op1 st ((br1 . ia1 . uc) (tknCategory . utxo))
    OP_UTXOTOKENCOMMITMENT ->   op1 st ((br1 . ia1 . uc) (tknCommitment . utxo))
    OP_UTXOTOKENAMOUNT ->       op1 st ((ir1 . ia1 . uc) (tknAmount . utxo))
    OP_OUTPUTTOKENCATEGORY ->   op1 st ((br1 . ia1 . oc) (tknCategory . out))
    OP_OUTPUTTOKENCOMMITMENT -> op1 st ((br1 . ia1 . oc) (tknCommitment . out))
    OP_OUTPUTTOKENAMOUNT ->     op1 st ((ir1 . ia1 . oc) (tknAmount . out))
    _ -> Nothing
  where
    br1 :: (a -> Either ScriptError Bytes) ->
           (a -> Either ScriptError StackElement)
    br1 = VU.br1 st.params

    ia1 :: (Integer -> Either ScriptError a) ->
           (StackElement -> Either ScriptError a)
    ia1 = VU.ia1 st.params

    ir1 = VU.ir1 st.params

    tx = txContextTx txContext

    uc :: (Int -> a) -> (Integer -> Either ScriptError a)
    uc = indexCheck (length $ txContextCoins txContext)

    oc :: (Int -> a) -> (Integer -> Either ScriptError a)
    oc = indexCheck (length tx.outputs)

    utxo = (\idx -> txContextCoins txContext !! idx) :: Int -> TxOut

    out = (tx.outputs !!)
{- ORMOLU_ENABLE -}

tknAmount :: TxOut -> Word64
tknAmount TxOut {tokenData = Nothing} = 0
tknAmount TxOut {tokenData = Just (TokenData {amount})} = amount

tknCategory :: TxOut -> ByteString
tknCategory TxOut {tokenData = Nothing} = B.empty
tknCategory TxOut {tokenData = Just (TokenData {category, nft = Nothing})} =
  category.id.hash
tknCategory
  TxOut
    { tokenData = Just (TokenData {category, nft = Just (Nft {capability})})
    } =
    category.id.hash <> case capability of
      None -> B.empty
      Mutable -> B.singleton capabilityMutable
      Minting -> B.singleton capabilityMinting
    where
      capabilityMutable = 1
      capabilityMinting = 2

tknCommitment :: TxOut -> ByteString
tknCommitment TxOut {tokenData = Nothing} = B.empty
tknCommitment TxOut {tokenData = Just (TokenData {nft = Nothing})} = B.empty
tknCommitment
  TxOut {tokenData = Just (TokenData {nft = Just (Nft {commitment})})} =
    commitment
