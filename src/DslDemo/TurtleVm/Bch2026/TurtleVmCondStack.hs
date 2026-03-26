-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmCondStack
  ( executeP,
    condStackExecuteP,
  )
where

import Alba.Dsl.V1.Bch2026
  ( FN,
    N,
    TBool,
    TBytes,
    begin,
    bytes,
    del,
    function,
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
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Maybe (TMaybe, ifJust)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (isConditionalOp, isSingleByteOp)
import Prelude hiding (and)

executeP ::
  FN
    (s > N "op" (TMaybe TBytes) > N "condStack" TBytes)
    (s > TBytes > TBool)
executeP =
  begin
    # (roll "op")
    # ifJust
      ( begin
          # isSingleByteOp
          # opIf
            ( begin
                # (opDup # isConditionalOp)
                # (roll "condStack" # condStackExecuteP)
                # opBoolOr
            )
            (roll "condStack" # condStackExecuteP)
      )
      (del "condStack" # bytes [] # opFalse)

condStackExecuteP :: FN (s > TBytes) (s > TBool)
condStackExecuteP = function (opTrue # opSwap # opUntil loop # opDrop)
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
