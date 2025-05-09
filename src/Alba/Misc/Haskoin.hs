-- Copyright (c) 2025 albaDsl

module Alba.Misc.Haskoin
  ( H.Address (..),
    H.Network (..),
    H.VarInt (..),
    H.SigHashType (..),
    H.TxSignature (..),
    H.PublicKey (..),
    H.PrivateKey (..),
    H.Base58,
    H.textToAddr,
    H.addrToText,
    H.mainnet,
    H.chipnet,
    H.putVarInt,
    H.anyoneCanPay,
    H.hasUtxosFlag,
    H.isSigHashNone,
    H.isSigHashSingle,
    H.setForkIdFlag,
    H.sigHashAll,
    H.marshal,
    H.wrapPubKey,
    H.pubKeyAddr,
    H.toWif,
    H.fromWif,
    signHash,
    scriptAddress,
    addressHash,
  )
where

import Alba.Tx.Bch2025.Hash (Hash256 (..))
import Crypto.Secp256k1 (Ctx, SecKey, Sig)
import Data.ByteString qualified as B
import Data.ByteString.Short (fromShort, toShort)
import Haskoin qualified as H

signHash :: Ctx -> SecKey -> Hash256 -> Sig
signHash ctx secKey (Hash256 bs) =
  H.signHash ctx secKey (H.Hash256 (toShort bs))

scriptAddress :: B.ByteString -> H.Address
scriptAddress bs = H.ScriptAddress (H.hash160 bs)

addressHash :: H.Address -> B.ByteString
addressHash (H.PubKeyAddress hash) = fromShort hash.get
addressHash (H.ScriptAddress hash) = fromShort hash.get
