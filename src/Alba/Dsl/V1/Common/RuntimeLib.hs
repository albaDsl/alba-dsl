-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.RuntimeLib (toPushOp) where

import Alba.Dsl.V1.Bch2025.Lang (bytes, case', nat)
import Alba.Dsl.V1.Bch2025.Ops
  ( op2Drop,
    opAdd,
    opCat,
    opDrop,
    opFalse,
    opLessThanOrEqual,
    opNumEqual,
    opRot,
    opSize,
    opSplit,
    opSwap,
    opVerify,
    opWithin,
  )
import Alba.Dsl.V1.Bch2026.Lang (fn)
import Alba.Dsl.V1.Bch2026.Stack (TCode)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.Stack (Fn, TBool, TBytes, TNat, cast)
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..))
import Numeric.Natural (Natural)

-- Turns a byte value into an instruction for pushing that byte value. ToPushOp
-- is 98 bytes in size.
toPushOp :: Fn (s > TBytes) (s > TCode)
toPushOp =
  fn
    ( begin
        # opSize
        # case'
          [ (is 0x00, op2Drop # opcode OP_0),
            ( is 0x01,
              begin
                # (opDrop # b2n)
                # case'
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
        # b2c
    )
  where
    is :: Natural -> Fn (s > TNat) (s > TBool)
    is x = nat x # opNumEqual

    lessOrEq :: Natural -> Fn (s > TNat) (s > TBool)
    lessOrEq x = nat x # opLessThanOrEqual

    inRange :: Natural -> Natural -> Fn (s > TNat) (s > TBool)
    inRange x y = nat x # nat y # opWithin

    b2n :: Fn (s > TBytes) (s > TNat)
    b2n = cast

    n2b :: Fn (s > TNat) (s > TBytes)
    n2b = cast

    b2c :: Fn (s > TBytes) (s > TCode)
    b2c = cast

    opcode :: OpcodeL1 -> Fn s (s > TBytes)
    opcode op = bytes [(fromIntegral . fromEnum) op]

    dropSign :: Fn (s > TNat) (s > TBytes)
    dropSign = n2b # nat 1 # opSplit # opDrop

    assemblePushData :: Fn (s > TBytes > TBytes > TBytes) (s > TBytes)
    assemblePushData = opSwap # opRot # opCat # opCat
