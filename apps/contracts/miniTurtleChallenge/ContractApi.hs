-- Copyright (c) 2025 albaDsl

module ContractApi (instantiate, withdraw) where

import Alba.Dsl.V1.Bch2025
  ( CodeL1,
    Contract (MkContract),
    Fn,
    Optimize (None, O1),
    Stack (..),
    TBytes,
    bytes,
    compile,
    (.),
  )
import Alba.Misc.Haskoin (Address (..), scriptAddress)
import Contract (contract)
import Crypto.Secp256k1 (Ctx)
import Prelude hiding ((.))

instantiate :: Ctx -> (CodeL1, Address)
instantiate _ctx =
  let (MkContract script) = contract
      script' = compile O1 script
      addr = scriptAddress script'
   in (script', addr)

withdraw :: Ctx -> CodeL1 -> CodeL1 -> CodeL1
withdraw _ctx solution redeemScript = compile None args
  where
    args :: Fn s (s :> TBytes :> TBytes)
    args = bytes solution . bytes redeemScript
