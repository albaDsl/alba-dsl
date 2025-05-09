-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpIntrospection (evalOpIntrospection) where

import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025.TxContext
  ( TxContext,
    txContextCoins,
    txContextInputIndex,
    txContextTx,
  )
import Alba.Vm.Bch2025.Utils (indexCheck, nc0, op0, op1)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (StackElement)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Binary (encode)
import Data.ByteString.Lazy (toStrict)

{- ORMOLU_DISABLE -}
evalOpIntrospection ::
  OpcodeL2 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpIntrospection op txContext st@VmState {..} =
  case op of
    OP_ACTIVEBYTECODE -> op0 st ((br0 . nc0) signedCode)
    OP_INPUTINDEX ->     op0 st ((ir0 . nc0) (txContextInputIndex txContext))
    OP_TXVERSION ->      op0 st ((ir0 . nc0) tx.version)
    OP_TXLOCKTIME ->     op0 st ((ir0 . nc0) tx.lockTime)
    OP_TXINPUTCOUNT ->   op0 st ((ir0 . nc0) (length tx.inputs))
    OP_TXOUTPUTCOUNT ->  op0 st ((ir0 . nc0) (length tx.outputs))
    OP_UTXOVALUE ->      op1 st ((ir1 . ia1 . ic) (\i -> (cn i).value))
    OP_UTXOBYTECODE ->   op1 st ((br1 . ia1 . ic) (\i -> (cn i).scriptPubKey))
    OP_OUTPOINTTXHASH -> op1 st ((br1 . ia1 . ic) (\i -> enc (inp i).prevout.txId))
    OP_OUTPOINTINDEX ->  op1 st ((ir1 . ia1 . ic) (\i -> (inp i).prevout.index))
    OP_INPUTBYTECODE ->  op1 st ((br1 . ia1 . ic) (\i -> (inp i).scriptSig))
    OP_INPUTSEQUENCENUMBER -> op1 st ((ir1 . ia1 . ic) (\i -> (inp i).sequence))
    OP_OUTPUTVALUE ->    op1 st ((ir1 . ia1 . oc) (\i -> (out i).value))
    OP_OUTPUTBYTECODE -> op1 st ((br1 . ia1 . oc) (\i -> (out i).scriptPubKey))
    _ -> Nothing
  where
    br0 = VU.br0 st.params
    br1 = VU.br1 st.params

    ia1 :: (Integer -> Either ScriptError a) ->
           (StackElement -> Either ScriptError a)
    ia1 = VU.ia1 st.params

    ir0 :: (Integral b) => Either ScriptError b ->
           Either ScriptError StackElement
    ir0 = VU.ir0 st.params

    ir1 :: (Integral b) =>
           (a -> Either ScriptError b) ->
           (a -> Either ScriptError StackElement)
    ir1 = VU.ir1 st.params

    tx = txContextTx txContext :: Tx

    ic :: (Int -> a) -> (Integer -> Either ScriptError a)
    ic = indexCheck (length tx.inputs)

    oc :: (Int -> a) -> (Integer -> Either ScriptError a)
    oc = indexCheck (length tx.outputs)

    inp = (tx.inputs !!)
    out = (tx.outputs !!)
    cn = (txContextCoins txContext !!)
    enc = toStrict . encode
{- ORMOLU_ENABLE -}
