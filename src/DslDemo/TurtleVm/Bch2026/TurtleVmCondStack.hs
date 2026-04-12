-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmCondStack
  ( executeP,
    condStackExecuteP,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    N,
    TBool,
    TBytes,
    begin,
    bytes,
    del,
    fn,
    nat,
    opBoolOr,
    opEqual,
    opFalse,
    opGreaterThanOrEqual,
    opIf,
    opSize,
    opSplit,
    opTrue,
    opUntil,
    roll,
    (∘),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, rot, swap)
import Alba.Dsl.V1.Bch2026.Contract.TMaybe (TMaybe, ifJust)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (isConditionalOp, isSingleByteOp)
import Prelude hiding (drop)

executeP ::
  Fn
    (s > N "op" (TMaybe TBytes) > N "condStack" TBytes)
    (s > TBytes > TBool)
executeP =
  begin
    ∘ (roll #op)
    ∘ ifJust
      ( begin
          ∘ isSingleByteOp
          ∘ opIf
            ( begin
                ∘ (dup ∘ isConditionalOp)
                ∘ (roll #condStack ∘ condStackExecuteP)
                ∘ opBoolOr
            )
            (roll #condStack ∘ condStackExecuteP)
      )
      (del #condStack ∘ bytes [] ∘ opFalse)

condStackExecuteP :: Fn (s > TBytes) (s > TBool)
condStackExecuteP = fn (opTrue ∘ swap ∘ opUntil loop ∘ drop)
  where
    loop :: Loop (s > TBool > TBytes)
    loop =
      begin
        ∘ (opSize ∘ nat 1 ∘ opGreaterThanOrEqual)
        ∘ opIf
          ( begin
              ∘ (nat 1 ∘ opSplit ∘ swap ∘ bytes [1] ∘ opEqual)
              ∘ opIf
                (opTrue ∘ replaceResult ∘ opFalse)
                (opFalse ∘ replaceResult ∘ opTrue)
          )
          (opTrue ∘ replaceResult ∘ opTrue)

    replaceResult :: Fn (s > TBool > TBytes > TBool) (s > TBool > TBytes)
    replaceResult = rot ∘ drop ∘ swap
