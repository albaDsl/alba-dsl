-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.RuntimeLibUntyped (invoke) where

import Alba.Dsl.V1.Bch2026.LangUntyped (bytes, fn, int)
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
  )
import Alba.Dsl.V1.Common.OpcodeL3 (localIdMaxLength)
import Alba.Dsl.V1.Common.StackUntyped (FnU, (.))
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

-- Evaluator that ONLY handles OP_PUSHDATA2 and OP_INVOKE. It does not enforce
-- the minimal push requirement. OP_INVOKEed code can use-and-restore but not
-- modify the alt-stack. Code to evaluate expected at top of stack.
eval :: FnU
eval =
  fn
    ( opUntil
        ( (opDup . bytes [] . opEqual)
            . opIf
              (opDrop . opTrue)
              ( (int 1 . opSplit . opSwap . bytes pushdataOp . opEqual)
                  . opIf
                    (int 2 . opSplit . opSwap . toSigned . opSplit)
                    (opToAltStack . opInvoke . opFromAltStack)
                  . opFalse
              )
        )
    )
  where
    pushdataOp :: B.ByteString
    pushdataOp = [0x4d]

    toSigned :: FnU
    toSigned = bytes [0] . opCat . opBin2Num
