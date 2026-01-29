-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.RuntimeLib (toPushOp) where

import Alba.Dsl.V1.Bch2025.Lang
import Alba.Dsl.V1.Bch2025.Ops
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang
import Alba.Dsl.V1.Common.Stack (FN, TBool, TBytes, TNat)
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..))
import Numeric.Natural (Natural)

-- Turns a byte value into an instruction for pushing that byte value. ToPushOp
-- is 98 bytes in size.
toPushOp :: FN (s > TBytes) (s > TBytes)
toPushOp =
  begin
    # opSize
    # cond
      [ (is 0x00, op2Drop # opcode OP_0),
        ( is 0x01,
          begin
            # (opDrop # b2n)
            # cond
              [ (inRange 0x01 0x11, opcode OP_RESERVED # b2n # opAdd # n2b),
                (is 0x81, opDrop # opcode OP_1NEGATE)
              ]
              (n2b # bytes [0x01] # opSwap # opCat)
        ),
        (lessOrEq 0x4b, n2b # opSwap # opCat),
        (lessOrEq 0x7f, n2b # opcode OP_PUSHDATA1 # assemblePushData),
        (lessOrEq 0xff, dropSign # opcode OP_PUSHDATA1 # assemblePushData),
        (lessOrEq 9997, n2b # opcode OP_PUSHDATA2 # assemblePushData)
      ]
      (opDrop # opFalse # opVerify)
  where
    is :: Natural -> FN (s > TNat) (s > TBool)
    is x = nat x # opNumEqual

    lessOrEq :: Natural -> FN (s > TNat) (s > TBool)
    lessOrEq x = nat x # opLessThanOrEqual

    inRange :: Natural -> Natural -> FN (s > TNat) (s > TBool)
    inRange x y = nat x # nat y # opWithin

    b2n :: FN (s > TBytes) (s > TNat)
    b2n = cast

    n2b :: FN (s > TNat) (s > TBytes)
    n2b = cast

    opcode :: OpcodeL1 -> FN s (s > TBytes)
    opcode op = bytes [(fromIntegral . fromEnum) op]

    dropSign :: FN (s > TNat) (s > TBytes)
    dropSign = n2b # nat 1 # opSplit # opDrop

    assemblePushData :: FN (s > TBytes > TBytes > TBytes) (s > TBytes)
    assemblePushData = opSwap # opRot # opCat # opCat
