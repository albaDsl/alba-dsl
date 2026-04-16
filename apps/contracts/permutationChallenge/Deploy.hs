-- Copyright (c) 2026 albaDsl

module Deploy (deployTx) where

import Alba.Dsl.V1.Bch2026
  ( Bytes,
    CodeL1,
    Fn,
    Optimize (None),
    TPubKey,
    TSig,
    bytes',
    compile,
    setScriptSig,
    signAll,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Misc.Bchn (getTx)
import Alba.Misc.Haskoin (Network, marshal, wrapPubKey)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Misc.Wallet (getWallet)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import ContractApi (instantiate)
import Crypto.Secp256k1 (Ctx)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Params (deployFee)
import Prelude hiding ((.))

-- outpoint: Points at the UTXO holding funds under alice's key.
deployTx :: Ctx -> Network -> OutPoint -> IO Tx
deployTx ctx net outpoint = do
  alice <- fromMaybe err1 <$> getWallet net "alice"
  walletTx <- either err2 id <$> getTx net outpoint.txId
  code <- instantiate
  let utxo = walletTx.outputs !! (fromIntegral outpoint.index)
      tx = txTemplate code outpoint utxo.value
      pubKey' = marshal ctx (wrapPubKey False alice.pubKey)
      sig = marshal ctx (signAll ctx tx utxo.scriptPubKey utxo 0 alice.secKey)
  pure $ setScriptSig 0 (compile None (scriptSig sig pubKey')) tx
  where
    err1 = error "Failed to load keys."

    err2 = error "Couldn't load wallet funding Tx."

    scriptSig :: Bytes -> Bytes -> Fn s (s > TPubKey > TSig)
    scriptSig pubKey sig = bytes' pubKey . bytes' sig

txTemplate :: CodeL1 -> OutPoint -> Word64 -> Tx
txTemplate code outpoint walletAmount =
  let libOutputs = Dc.deployTx.outputs <> Vc.deployTx.outputs
      contractAmount = walletAmount - sum ((.value) <$> libOutputs) - deployFee
   in Tx
        { version = 2,
          inputs = [TxIn {prevout = outpoint, scriptSig = [], sequence = 0}],
          outputs =
            libOutputs
              <> [ TxOut
                     { value = contractAmount,
                       scriptPubKey = code,
                       tokenData = Nothing
                     }
                 ],
          lockTime = 0
        }
