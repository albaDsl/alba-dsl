-- Copyright (c) 2026 albaDsl

module Deploy (deployTx) where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    FN,
    Optimize (None),
    TPubKey,
    TSig,
    bytes',
    compile,
    outputScript,
    setScriptSig,
    signAll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Misc.Haskoin (Network, marshal, pubKeyAddr, wrapPubKey)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Wallet (getWallet)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx, PubKey)
import Data.Maybe (fromMaybe)
import Params (contractAmount, deployAmount)

-- outpoint: Points at the UTXO holding funds under alice's key.
deployTx :: Ctx -> Network -> OutPoint -> IO Tx
deployTx ctx net outpoint = do
  alice@KeyPair {..} <- fromMaybe err <$> getWallet net "alice"
  let tx = txTemplate outpoint
      utxo = fundsSource pubKey
      pubKey' = marshal ctx (wrapPubKey False alice.pubKey)
      sig = marshal ctx (signAll ctx tx utxo.scriptPubKey utxo 0 alice.secKey)
  pure $ setScriptSig 0 (compile None (scriptSig sig pubKey')) tx
  where
    err = error "Failed to load keys."

    fundsSource :: PubKey -> TxOut
    fundsSource pubKey = do
      let recvAddr = pubKeyAddr ctx (wrapPubKey False pubKey)
      TxOut
        { value = deployAmount,
          scriptPubKey = outputScript recvAddr,
          tokenData = Nothing
        }

    scriptSig :: Bytes -> Bytes -> FN s (s > TPubKey > TSig)
    scriptSig pubKey sig = bytes' pubKey # bytes' sig

txTemplate :: OutPoint -> Tx
txTemplate outpoint =
  Tx
    { version = 2,
      inputs = [TxIn {prevout = outpoint, scriptSig = [], sequence = 0}],
      outputs =
        Dc.deployTx.outputs
          <> Vc.deployTx.outputs
          <> [ TxOut
                 { value = contractAmount,
                   scriptPubKey = instantiate,
                   tokenData = Nothing
                 }
             ],
      lockTime = 0
    }
