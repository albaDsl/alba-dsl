-- Copyright (c) 2025 albaDsl

module TestUtils where

import Alba.Dsl.V1.Bch2025 (FNA, Optimize (..), compile, outputScript)
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025 qualified as Bch2025
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.Common
  ( CodeL1,
    ScriptError,
    TxContext,
    VmStack,
    VmState (..),
    mkTxContext,
  )
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Sequence qualified as S

evaluateProg :: (FNA s '[] s' alt') -> Either ScriptError (VmStack, VmStack)
evaluateProg prog = evaluateProgWithStack prog (S.empty, S.empty)

evaluateProgWithStack ::
  FNA s '[] s' alt' ->
  (VmStack, VmStack) ->
  Either ScriptError (VmStack, VmStack)
evaluateProgWithStack prog (s, alt) =
  evaluateProgWithParams prog (s, alt) context
  where
    context = fromJust $ mkTxContext barboneTx 0 undefined

    barboneTx =
      Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}

evaluateProgWithParams ::
  FNA s '[] s' alt' ->
  (VmStack, VmStack) ->
  TxContext ->
  Either ScriptError (VmStack, VmStack)
evaluateProgWithParams prog = evaluateScript (compile None prog)

evaluateScript ::
  CodeL1 ->
  (VmStack, VmStack) ->
  TxContext ->
  Either ScriptError (VmStack, VmStack)
evaluateScript code (s, alt) context = do
  let res2025 =
        let state = (Bch2025.startState Bch2025.vmParamsStandard) {code, s, alt}
         in Bch2025.evaluateScript context state
      res2026 =
        let state = (Bch2026.startState Bch2026.vmParamsStandard) {code, s, alt}
         in Bch2026.evaluateScript context state
  unless (resultsEqual res2025 res2026) $
    error "Bch2025 & Bch2026 results don't match."
  case res2025 of
    Left (err, _) -> Left err
    Right VmState {s = s', alt = alt'} -> Right (s', alt')
  where
    resultsEqual ::
      Either (ScriptError, Maybe VmState) VmState ->
      Either (ScriptError, Maybe VmState) VmState ->
      Bool
    resultsEqual (Left (err, Nothing)) (Left (err', Nothing)) = err == err'
    resultsEqual (Left (err, Just st)) (Left (err', Just st')) =
      err == err' && stateEqual st st'
    resultsEqual (Right st) (Right st') = stateEqual st st'
    resultsEqual _ _ = False

    stateEqual :: VmState -> VmState -> Bool
    stateEqual st st' =
      (st.s, st.alt, st.metrics) == (st'.s, st'.alt, st'.metrics)

txContext :: TxOut -> TxContext
txContext utxo = fromJust $ mkTxContext tx 0 [utxo]

tx :: Tx
tx =
  Tx
    { version = 2,
      inputs =
        [ TxIn
            { prevout = OutPoint {txId = mockTxId, index = 0},
              scriptSig = [],
              sequence = 0
            }
        ],
      outputs =
        [ TxOut
            { value = 10_000,
              scriptPubKey = scriptPubKey,
              tokenData = Nothing
            }
        ],
      lockTime = 0
    }
  where
    scriptPubKey :: B.ByteString
    scriptPubKey = outputScript mockAddr

utxoWithPubkey :: B.ByteString -> TxOut
utxoWithPubkey scriptPubKey =
  TxOut
    { value = 10_000,
      scriptPubKey = scriptPubKey,
      tokenData = Nothing
    }
