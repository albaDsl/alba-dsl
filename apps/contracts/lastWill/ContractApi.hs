-- Copyright (c) 2025 albaDsl

module ContractApi (instantiate, refresh, withdraw, inherit) where

import Alba.Dsl.V1.Bch2025
import Alba.Misc.Haskoin
  ( Address (..),
    TxSignature,
    marshal,
    scriptAddress,
    wrapPubKey,
  )
import Alba.Tx.Bch2025 (hash160)
import Contract (Params, contract)
import Crypto.Secp256k1 (Ctx, PubKey)
import Numeric.Natural (Natural)

instantiate :: Ctx -> PubKey -> PubKey -> PubKey -> (CodeL1, Address)
instantiate ctx refreshPk withdrawPk inheritPk =
  let (MkContract script) = contract
      script' = compile None params <> compile O1 script
      addr = scriptAddress script'
   in (script', addr)
  where
    params :: FN Base Params
    params =
      let marshalPk pk = marshal ctx (wrapPubKey False pk)
          refreshHash = (hash160 . marshalPk) refreshPk
          withdrawHash = (hash160 . marshalPk) withdrawPk
          inheritHash = (hash160 . marshalPk) inheritPk
       in begin
            # name @"refreshHash" (bytes' refreshHash)
            # name @"withdrawHash" (bytes' withdrawHash)
            # name @"inheritHash" (bytes' inheritHash)

refresh :: Ctx -> CodeL1 -> PubKey -> TxSignature -> CodeL1
refresh = scriptSig 0

withdraw :: Ctx -> CodeL1 -> PubKey -> TxSignature -> CodeL1
withdraw = scriptSig 1

inherit :: Ctx -> CodeL1 -> PubKey -> TxSignature -> CodeL1
inherit = scriptSig 2

scriptSig :: Natural -> Ctx -> CodeL1 -> PubKey -> TxSignature -> CodeL1
scriptSig fn ctx redeemScript pubKey sig = compile None args
  where
    args :: FN s (s > TPubKey > TSig > TNat > TBytes)
    args =
      let pk = marshal ctx (wrapPubKey False pubKey)
          s = marshal ctx sig
       in bytes' pk # bytes' s # nat fn # bytes redeemScript
