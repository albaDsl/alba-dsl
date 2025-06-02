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
import Data.Foldable (toList)
import Data.Sequence qualified as S

type VmStack = S.Seq StackElement

type CondStack = S.Seq CondStackElement

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
condStackEmpty = S.empty

condStackNull :: CondStack -> Bool
condStackNull = S.null

condStackSize :: CondStack -> Int
condStackSize = S.length

condStackPush :: CondStack -> CondStackElement -> CondStack
condStackPush s element = s S.|> element

condStackUncons :: CondStack -> (Maybe CondStackElement, CondStack)
condStackUncons (rest S.:|> x) = (Just x, rest)
condStackUncons rest = (Nothing, rest)

condStackToggle :: CondStack -> Maybe CondStack
condStackToggle s =
  case s of
    (rest S.:|> Exec top) -> Just $ rest S.|> Exec (not top)
    _ -> Nothing

condStackDrop :: CondStack -> Maybe CondStack
condStackDrop s =
  case s of
    (rest S.:|> Exec _top) -> Just rest
    _ -> Nothing

condStackExecuteP :: CondStack -> Bool
condStackExecuteP = all execVal . takeWhile isExec . toList . S.reverse
  where
    execVal :: CondStackElement -> Bool
    execVal (Exec x) = x
    execVal (Eval _ _) = canNotHappen

    isExec :: CondStackElement -> Bool
    isExec (Exec _) = True
    isExec (Eval _ _) = False
