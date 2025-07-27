-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleOpCryptography (turtleOpCryptography) where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( opCheckMultiSig,
    opCheckMultiSigVerify,
    opCheckSig,
    opCheckSigVerify,
    opHash160,
    opHash256,
    opRipemd160,
    opSha1,
    opSha256,
  )
import Alba.Dsl.V1.Common.StackUntyped (FNU)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped (condOpLeaf, is)

turtleOpCryptography :: FNU
turtleOpCryptography =
  condOpLeaf
    [ (is 0xa6, opRipemd160),
      (is 0xa7, opSha1),
      (is 0xa8, opSha256),
      (is 0xa9, opHash160),
      (is 0xaa, opHash256),
      -- (is 0xab, opCodeSeparator), -- Not implemented.
      (is 0xac, opCheckSig),
      (is 0xad, opCheckSigVerify),
      (is 0xae, opCheckMultiSig),
      (is 0xaf, opCheckMultiSigVerify)
    ]
