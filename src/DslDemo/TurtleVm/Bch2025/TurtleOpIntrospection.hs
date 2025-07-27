-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleOpIntrospection
  ( turtleOpIntrospection,
  )
where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( opInputBytecode,
    opInputIndex,
    opInputSequenceNumber,
    opOutPointIndex,
    opOutPointTxHash,
    opOutputBytecode,
    opOutputTokenAmount,
    opOutputTokenCategory,
    opOutputTokenCommitment,
    opOutputValue,
    opTxInputCount,
    opTxLockTime,
    opTxOutputCount,
    opTxVersion,
    opUtxoBytecode,
    opUtxoTokenAmount,
    opUtxoTokenCategory,
    opUtxoTokenCommitment,
    opUtxoValue,
  )
import Alba.Dsl.V1.Common.StackUntyped (FNU)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped
  ( condOp,
    condOpLeaf,
    inRange,
    is,
  )

turtleOpIntrospection :: FNU
turtleOpIntrospection =
  condOp
    [ ( inRange 0xc0 0xca,
        condOpLeaf
          [ (is 0xc0, opInputIndex),
            -- (is 0xc1, opActiveBytecode), -- Not implemented.
            (is 0xc2, opTxVersion),
            (is 0xc3, opTxInputCount),
            (is 0xc4, opTxOutputCount),
            (is 0xc5, opTxLockTime),
            (is 0xc6, opUtxoValue),
            (is 0xc7, opUtxoBytecode),
            (is 0xc8, opOutPointTxHash),
            (is 0xc9, opOutPointIndex)
          ]
      ),
      ( inRange 0xca 0xd4,
        condOpLeaf
          [ (is 0xca, opInputBytecode),
            (is 0xcb, opInputSequenceNumber),
            (is 0xcc, opOutputValue),
            (is 0xcd, opOutputBytecode),
            (is 0xce, opUtxoTokenCategory),
            (is 0xcf, opUtxoTokenCommitment),
            (is 0xd0, opUtxoTokenAmount),
            (is 0xd1, opOutputTokenCategory),
            (is 0xd2, opOutputTokenCommitment),
            (is 0xd3, opOutputTokenAmount)
          ]
      )
    ]
