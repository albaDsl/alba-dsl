-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.RuntimeLib (toPushOp) where

import Alba.Dsl.V1.Bch2026.Lang (bytes, case', fn, nat)
import Alba.Dsl.V1.Bch2026.Ops
  ( op2Drop,
    opAdd,
    opAnd,
    opCat,
    opDrop,
    opEqual,
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
import Alba.Dsl.V1.Bch2026.Stack (TCode)
import Alba.Dsl.V1.Common.Lang (begin, (∘))
import Alba.Dsl.V1.Common.Stack (Fn, Stack (..), TBool, TBytes, TNat, cast)
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..))
import Data.ByteString qualified as B
import Numeric.Natural (Natural)

-- Turns a byte value into an instruction for pushing that byte value. ToPushOp
-- is 125 bytes in size.
toPushOp :: Fn (s :> TBytes) (s :> TCode)
toPushOp =
  fn
    ( begin
        ∘ opSize
        ∘ case'
          [ (numEq 0, op2Drop ∘ opcode OP_0),
            ( numEq 1,
              begin
                ∘ opDrop
                ∘ case'
                  [ (bytesEq [0x00], simpleOpData),
                    ( bytes [0x80] ∘ opAnd ∘ bytes [0] ∘ opEqual,
                      case'
                        [ ( b2n ∘ numInRange 1 17,
                            (b2n ∘ opcode OP_RESERVED ∘ b2n ∘ opAdd ∘ n2b)
                          )
                        ]
                        simpleOpData
                    ),
                    (bytesEq [0x81], opDrop ∘ opcode OP_1NEGATE)
                  ]
                  simpleOpData
            ),
            (lessOrEq 0x4b, n2b ∘ opSwap ∘ opCat),
            (lessOrEq 0x7f, n2b ∘ opcode OP_PUSHDATA1 ∘ assemblePushData),
            (lessOrEq 0xff, dropSign ∘ opcode OP_PUSHDATA1 ∘ assemblePushData),
            (lessOrEq 9997, n2b ∘ opcode OP_PUSHDATA2 ∘ assemblePushData)
          ]
          (opDrop ∘ opFalse ∘ opVerify)
        ∘ b2c
    )
  where
    simpleOpData :: forall s. Fn (s :> TBytes) (s :> TBytes)
    simpleOpData = bytes [0x01] ∘ opSwap ∘ opCat

    numEq :: Natural -> Fn (s :> TNat) (s :> TBool)
    numEq x = nat x ∘ opNumEqual

    numInRange :: Natural -> Natural -> Fn (s :> TNat) (s :> TBool)
    numInRange x y = nat x ∘ nat y ∘ opWithin

    lessOrEq :: Natural -> Fn (s :> TNat) (s :> TBool)
    lessOrEq x = nat x ∘ opLessThanOrEqual

    bytesEq :: B.ByteString -> Fn (s :> TBytes) (s :> TBool)
    bytesEq x = bytes x ∘ opEqual

    b2n :: Fn (s :> TBytes) (s :> TNat)
    b2n = cast

    n2b :: Fn (s :> TNat) (s :> TBytes)
    n2b = cast

    b2c :: Fn (s :> TBytes) (s :> TCode)
    b2c = cast

    opcode :: OpcodeL1 -> Fn s (s :> TBytes)
    opcode op = bytes [(fromIntegral . fromEnum) op]

    dropSign :: Fn (s :> TNat) (s :> TBytes)
    dropSign = n2b ∘ nat 1 ∘ opSplit ∘ opDrop

    assemblePushData :: Fn (s :> TBytes :> TBytes :> TBytes) (s :> TBytes)
    assemblePushData = opSwap ∘ opRot ∘ opCat ∘ opCat
