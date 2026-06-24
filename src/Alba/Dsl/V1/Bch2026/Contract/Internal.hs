module Alba.Dsl.V1.Bch2026.Contract.Internal where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    TBytes,
    bytes,
    n2i,
    nat,
    opCat,
    opNum2Bin,
    opSize,
    opSwap,
    (.),
  )
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..), opcodeL1ToWord8)
import Prelude ()

toPushData2Op :: Fn (s :> TBytes) (s :> TBytes)
toPushData2Op =
  (opSize . n2i . nat 2 . opNum2Bin . opSwap . opCat . opcode OP_PUSHDATA2)
    . (opSwap . opCat)

opcode :: OpcodeL1 -> Fn s (s :> TBytes)
opcode op = bytes [opcodeL1ToWord8 op]
