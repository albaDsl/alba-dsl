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
    condStackToggle,
    condStackDrop,
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
  | Eval
      { cseCode :: !CodeL1,
        cseSignedCode :: !CodeL1
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

condStackToggle :: CondStack -> Maybe CondStack
condStackToggle s =
  case s of
    (CondStack (Exec top : s') count) -> Just (CondStack (Exec (not top) : s') count)
    _ -> Nothing

condStackDrop :: CondStack -> Maybe CondStack
condStackDrop s =
  case s of
    (CondStack (Exec _top : s') count) -> Just (CondStack s' (pred count))
    _ -> Nothing

{-# INLINE condStackExecuteP #-}
condStackExecuteP :: CondStack -> Bool
condStackExecuteP (CondStack s _) = all execVal . takeWhile isExec $ s
  where
    execVal :: CondStackElement -> Bool
    execVal (Exec x) = x
    execVal (Eval _ _) = canNotHappen

    isExec :: CondStackElement -> Bool
    isExec (Exec _) = True
    isExec (Eval _ _) = False
