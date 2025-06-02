-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.VmStack
  ( VmStack,
    CondStack,
    CondStackElement (..),
    stackTop,
    stackInit,
    condStackEmpty,
    condStackNull,
    condStackSize,
    condStackPush,
    condStackUncons,
    condStackExecToggle,
    condStackExecDrop,
    condStackExecuteP,
  )
where

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.StackElement (StackElement)
import Data.Sequence qualified as S

type VmStack = S.Seq StackElement

data CondStack = CondStack ![CondStackElement] !Int
  deriving (Eq, Show)

data CondStackElement
  = Exec Bool
  | Loop
      { loopStart :: !CodeL1
      }
  | Eval
      { callerCode :: !CodeL1,
        callerSignedCode :: !CodeL1
      }
  deriving (Eq, Show)

stackTop :: VmStack -> Maybe StackElement
stackTop (_ S.:|> x) = Just x
stackTop _ = Nothing

stackInit :: VmStack -> Maybe VmStack
stackInit (xs S.:|> _) = Just xs
stackInit _ = Nothing

condStackEmpty :: CondStack
condStackEmpty = CondStack [] 0

condStackNull :: CondStack -> Bool
condStackNull (CondStack _ 0) = True
condStackNull (CondStack _ _) = False

condStackSize :: CondStack -> Int
condStackSize (CondStack _ count) = count

condStackPush :: CondStack -> CondStackElement -> CondStack
condStackPush (CondStack s count) element = CondStack (element : s) (succ count)

condStackUncons :: CondStack -> (Maybe CondStackElement, CondStack)
condStackUncons (CondStack (x : s) count) = (Just x, CondStack s (pred count))
condStackUncons _ = (Nothing, condStackEmpty)

condStackExecToggle :: CondStack -> Maybe CondStack
condStackExecToggle s =
  case s of
    (CondStack (Exec top : s') count) ->
      Just (CondStack (Exec (not top) : s') count)
    _ -> Nothing

condStackExecDrop :: CondStack -> Maybe CondStack
condStackExecDrop s =
  case s of
    (CondStack (Exec _top : s') count) -> Just (CondStack s' (pred count))
    _ -> Nothing

{-# INLINE condStackExecuteP #-}
condStackExecuteP :: CondStack -> Bool
condStackExecuteP (CondStack s _) = all doExec . takeWhile notEval $ s
  where
    doExec :: CondStackElement -> Bool
    doExec (Exec x) = x
    doExec (Loop _) = True
    doExec (Eval _ _) = canNotHappen

    notEval :: CondStackElement -> Bool
    notEval (Exec _) = True
    notEval (Loop _) = True
    notEval (Eval _ _) = False
