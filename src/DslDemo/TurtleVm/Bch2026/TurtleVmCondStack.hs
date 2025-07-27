-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmCondStack (executeP, condStackExecuteP) where

import Alba.Dsl.V1.Bch2026
  ( FN,
    N,
    TBool,
    TBytes,
    TInt,
    begin,
    bytes,
    drop,
    int,
    nat,
    opBoolOr,
    opDrop,
    opDup,
    opEqual,
    opFalse,
    opGreaterThanOrEqual,
    opIf,
    opRot,
    opSize,
    opSplit,
    opSwap,
    opTrue,
    opUntil,
    opWithin,
    roll,
    (#),
    type (>),
  )
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (toSigned)
import DslDemo.TurtleVm.Common.Maybe (TMaybe, ifJust)
import Prelude hiding (and, drop)

executeP ::
  FN
    (s > N "op" (TMaybe TBytes) > N "condStack" TBytes)
    (s > TInt > TBool)
executeP =
  begin
    # (roll @"op")
    # ifJust
      ( begin
          # toSigned
          # (opDup # isConditionalOp)
          # (roll @"condStack" # condStackExecuteP)
          # opBoolOr
      )
      (drop @"condStack" # int 0 # opFalse)

isConditionalOp :: FN (s > TInt) (s > TBool)
isConditionalOp = int 0x63 # int 0x69 # opWithin

condStackExecuteP :: FN (s > TBytes) (s > TBool)
condStackExecuteP = opTrue # opSwap # opUntil loop # opDrop
  where
    loop :: FN (s > TBool > TBytes) (s > TBool > TBytes > TBool)
    loop =
      begin
        # (opSize # nat 1 # opGreaterThanOrEqual)
        # opIf
          ( begin
              # (nat 1 # opSplit # opSwap # bytes [1] # opEqual)
              # opIf
                (opTrue # replaceResult # opFalse)
                (opFalse # replaceResult # opTrue)
          )
          (opTrue # replaceResult # opTrue)

    replaceResult :: FN (s > TBool > TBytes > TBool) (s > TBool > TBytes)
    replaceResult = opRot # opDrop # opSwap
