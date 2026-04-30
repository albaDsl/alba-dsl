-- Copyright (c) 2026 albaDsl

module ContractApi (instantiate, withdraw) where

import Alba.Dsl.V1.Bch2026 (bytes, nat, (.))
import Alba.Dsl.V1.Common.Compile
  ( CompilationResult (..),
    Optimize (None, O1),
    compile,
    compile',
    writeFunctionTable,
  )
import Alba.Dsl.V1.Common.Contract (Contract (MkContract))
import Alba.Misc.Haskoin (Address, scriptAddress)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Contract (contract)
import Crypto.Secp256k1 (Ctx)
import Data.ByteString qualified as B
import Numeric.Natural (Natural)
import Prelude hiding ((.))

instantiate :: IO (CodeL1, Address)
instantiate = do
  let (MkContract script) = contract
      res = compile' O1 script
      addr = scriptAddress res.code
  writeFunctionTable res
  pure (res.code, addr)

withdraw :: Ctx -> CodeL1 -> Natural -> CodeL1
withdraw _ctx redeemScript solution =
  compile None (nat solution . bytes filler . bytes redeemScript)
  where
    filler = B.replicate 6900 0
