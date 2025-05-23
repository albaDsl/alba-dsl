-- Copyright (c) 2025 albaDsl

module TestUtils where

import Alba.Dsl.V1.Bch2025 (FNA, Optimize (..), compile, outputScript)
import Alba.Misc.MockVals (mockAddr, mockTxId)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025 (TxContext, evaluateScript, mkTxContext, startState)
import Alba.Vm.Bch2025.VmParams (vmParamsStandard)
import Alba.Vm.Common (ScriptError, VmStack)
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Sequence qualified as S

evaluateProg :: (FNA s '[] s' alt') -> Either ScriptError (VmStack, VmStack)
evaluateProg prog = evaluateProgWithStack prog (S.empty, S.empty)

evaluateProgWithStack ::
  FNA s '[] s' alt' ->
  (VmStack, VmStack) ->
  Either ScriptError (VmStack, VmStack)
evaluateProgWithStack prog (s, alt) = do
  let code = compile None prog
      state = (startState vmParamsStandard) {s = s, alt = alt}
  case evaluateScript code context state of
    Left (err, _) -> Left err
    Right VmState {s = s', alt = alt'} -> Right (s', alt')
  where
    context = fromJust $ mkTxContext barboneTx 0 undefined
    barboneTx =
      Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}

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
