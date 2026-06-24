-- Copyright (c) 2026 albaDsl

module ContractApi (instantiate, withdraw) where

import Alba.Dsl.V1.Bch2026 (bytes, (.))
import Alba.Dsl.V1.Common.Compile
  ( CompilationResult (..),
    Optimize (None, O1),
    compile,
    compile',
    writeFunctionTable,
  )
import Alba.Dsl.V1.Common.Contract (Contract (MkContract))
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Contract (contract)
import Crypto.Secp256k1 (Ctx)
import Data.ByteString qualified as B
import Prelude hiding ((.))

instantiate :: IO CodeL1
instantiate = do
  let (MkContract script) = contract
      res = compile' O1 script
  writeFunctionTable res
  pure res.code

withdraw :: Ctx -> Bytes -> CodeL1
withdraw _ctx solution = compile None (bytes solution . bytes filler)
  where
    filler = B.replicate 7000 0
