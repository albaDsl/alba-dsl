-- Copyright (c) 2025 albaDsl

module ContractApi (instantiate, recipientWithdraw, senderWithdraw) where

import Alba.Dsl.V1.Bch2025
import Alba.Misc.Haskoin
  ( Address (..),
    TxSignature,
    marshal,
    scriptAddress,
    wrapPubKey,
  )
import Contract (Params, contract)
import Crypto.Secp256k1 (Ctx, PubKey)
import Numeric.Natural (Natural)

instantiate :: Ctx -> PubKey -> PubKey -> Natural -> (CodeL1, Address)
instantiate ctx senderPub recipientPub timeout =
  let (MkContract script) = contract
      script' = compile None params <> compile O1 script
      addr = scriptAddress script'
   in (script', addr)
  where
    params :: FN Base Params
    params =
      let senderPub' = marshal ctx (wrapPubKey False senderPub)
          recipientPub' = marshal ctx (wrapPubKey False recipientPub)
       in begin
            # name @"senderPub" (pubKeyBytes senderPub')
            # name @"recipientPub" (pubKeyBytes recipientPub')
            # name @"timeout" (nat timeout)

recipientWithdraw :: Ctx -> CodeL1 -> TxSignature -> CodeL1
recipientWithdraw = scriptSig 0

senderWithdraw :: Ctx -> CodeL1 -> TxSignature -> CodeL1
senderWithdraw = scriptSig 1

scriptSig :: Natural -> Ctx -> CodeL1 -> TxSignature -> CodeL1
scriptSig fn ctx redeemScript sig = compile None args
  where
    args :: FN s (s > TBytes > TNat > TBytes)
    args =
      let s = marshal ctx sig
       in bytes s # nat fn # bytes redeemScript
