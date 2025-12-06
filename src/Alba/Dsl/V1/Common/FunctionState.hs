-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    FunctionTable,
    Function (..),
    FunctionId (..),
    startState,
    registerFunction,
    addFunctionBody,
    addCallSite,
    isRegistered,
    getSlot,
    getCallerFunctionId,
    getCallerLambdaId,
    functionsSorted,
    functionsSortedSlot,
  )
where

import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId (..))
import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)

newtype FunctionState = FunctionState
  { functions :: FunctionTable
  }
  deriving (Eq, Show)

data Function = Function
  { code :: Maybe CodeL3,
    slot :: Maybe Int,
    callSites :: Int
  }
  deriving (Eq, Show)

type FunctionTable = M.Map FunctionId Function

startState :: FunctionState
startState = FunctionState {functions = M.empty}

registerFunction :: FunctionId -> FunctionState -> Maybe FunctionState
registerFunction fId fs@FunctionState {functions} =
  if not (M.member fId functions)
    then Just $
      case fId of
        Standard {} ->
          fs {functions = M.insert fId (Function Nothing Nothing 1) functions}
        Lambda {} ->
          fs {functions = M.insert fId (Function Nothing Nothing 0) functions}
        Named _ ->
          fs {functions = M.insert fId (Function Nothing Nothing 0) functions}
        Absolute slot ->
          let slot' = Just slot
           in fs {functions = M.insert fId (Function Nothing slot' 0) functions}
    else Nothing

addFunctionBody :: FunctionId -> CodeL3 -> FunctionState -> Maybe FunctionState
addFunctionBody fId code fs@FunctionState {functions} =
  case M.lookup fId functions of
    Just f ->
      Just $
        fs
          { functions = M.insert fId (f {code = Just code}) functions
          }
    Nothing -> Nothing

addCallSite :: FunctionId -> FunctionState -> Maybe FunctionState
addCallSite fId fs@FunctionState {functions} =
  case M.lookup fId functions of
    Just (Function {..}) ->
      Just $
        fs
          { functions =
              M.insert fId (Function {callSites = succ callSites, ..}) functions
          }
    Nothing -> Nothing

isRegistered :: FunctionId -> FunctionState -> Bool
isRegistered fId FunctionState {functions} = M.member fId functions

getSlot :: FunctionId -> FunctionState -> Maybe Int
getSlot fId FunctionState {functions} =
  case M.lookup fId functions of
    Just (Function {slot}) -> slot
    Nothing -> Nothing

getCallerFunctionId :: (HasCallStack) => Maybe FunctionId
getCallerFunctionId =
  let s = getCallStack callStack
   in case s of
        (_, loc) : (fun, _) : _ ->
          Just
            ( Standard
                (srcLocModule loc)
                (srcLocStartLine loc)
                loc.srcLocStartCol
                fun
            )
        _ -> Nothing

getCallerLambdaId :: (HasCallStack) => Maybe FunctionId
getCallerLambdaId = convert <$> getCallerFunctionId
  where
    convert :: FunctionId -> FunctionId
    convert (Standard moduleName line col fun) = Lambda moduleName line col fun
    convert _ = error ""

functionsSorted :: M.Map FunctionId Function -> [(FunctionId, Function)]
functionsSorted = M.toList >>> sortBy (flip compare `on` ((.callSites) . snd))

functionsSortedSlot :: M.Map FunctionId Function -> [(FunctionId, Function)]
functionsSortedSlot =
  M.toList >>> sortBy (compare `on` ((fromMaybe err1 . (.slot)) . snd))
  where
    err1 = error "functionsSortedSlot: internal error."
