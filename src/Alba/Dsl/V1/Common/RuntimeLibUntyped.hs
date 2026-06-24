-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.RuntimeLibUntyped (eval, invoke) where

import Alba.Dsl.V1.Bch2026.LangUntyped
import Alba.Dsl.V1.Bch2026.OpsUntyped
  ( opBin2Num,
    opCat,
    opDrop,
    opDup,
    opEqual,
    opFalse,
    opFromAltStack,
    opIf,
    opInvoke,
    opLessThanOrEqual,
    opSize,
    opSplit,
    opSwap,
    opToAltStack,
    opTrue,
    opUntil,
    opVerify,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (localIdMaxLength)
import Alba.Dsl.V1.Common.StackUntyped (FnU, SU, (.))
import Data.ByteString qualified as B
import Prelude (fromIntegral)

-- Args for the quotation followed by the type-A quotation itself expected on
-- the stack. If the size of the quotation bytestring is <= 'localIdMaxLength'
-- then it is just a standard function identifier in the local identifier space.
-- This works since a partially applied quotation, even if represented by the
-- minimal function identifier '[]' and partially applied with a minimal
-- argument, is always larger than 'localIdMaxLength'.
invoke :: FnU
invoke =
  fn
    ( (opSize . int (fromIntegral localIdMaxLength))
        . (opLessThanOrEqual . opIf opInvoke eval)
    )

-- Evaluator that only handles OP_PUSHDATA2 and OP_INVOKE. It does not enforce
-- the minimal push requirement. Code to evaluate expected at top of stack.
eval :: FnU
eval =
  fn
    ( opUntil
        ( (opDup . bytes [] . opEqual)
            . opIf
              (opDrop . opTrue)
              ( (int 1 . opSplit . opSwap)
                  . case'
                    [ ( bytesEq pushdataOp,
                        opDrop . int 2 . opSplit . opSwap . toSigned . opSplit
                      ),
                      ( bytesEq invokeOp,
                        opDrop . opToAltStack . opInvoke . opFromAltStack
                      )
                    ]
                    (bytes "E0" . opFalse . opVerify)
                  . opFalse
              )
        )
    )
  where
    bytesEq :: B.ByteString -> FnU
    bytesEq x = bytes x . opEqual

    pushdataOp :: B.ByteString
    pushdataOp = [0x4d]

    toSigned :: FnU
    toSigned = bytes [0] . opCat . opBin2Num

    invokeOp :: B.ByteString
    invokeOp = [0x8a]

case' :: [(SU -> SU, SU -> SU)] -> (SU -> SU) -> (SU -> SU)
case' [] def st = def st
case' ((test, result) : rest) def st =
  (opDup . test . opIf result (case' rest def)) st
