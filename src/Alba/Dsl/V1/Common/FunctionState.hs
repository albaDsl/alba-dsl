-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    FunctionTable,
    Function (..),
    startState,
    registerFunction,
    addFunctionBody,
    addCallSite,
    setCallSites,
    isRegistered,
    getCallerFunctionId,
    getCallerConstantId,
    getCallerRtConstantId,
    getCallerLambdaId,
    functionsSortedBySites,
  )
where

import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId (..))
import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map qualified as M
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)

newtype FunctionState = FunctionState
  { functions :: FunctionTable
  }
  deriving (Eq, Show)

data Function = Function
  { code :: Maybe CodeL3,
    index :: Maybe Int,
    callSites :: Int
  }
  deriving (Eq, Show)

type FunctionTable = M.Map FunctionId Function

startState :: FunctionState
startState = FunctionState {functions = M.empty}

registerFunction :: FunctionId -> FunctionState -> Maybe FunctionState
registerFunction fId fs@FunctionState {functions = fns} =
  if not (M.member fId fns)
    then Just $
      case fId of
        Standard {} ->
          fs {functions = M.insert fId (Function Nothing Nothing 1) fns}
        Constant {} ->
          fs {functions = M.insert fId (Function Nothing Nothing 1) fns}
        RuntimeConstant {} ->
          fs {functions = M.insert fId (Function Nothing Nothing 1) fns}
        Lambda {} ->
          fs {functions = M.insert fId (Function Nothing Nothing 1) fns}
        Named _ ->
          fs {functions = M.insert fId (Function Nothing Nothing 0) fns}
        Absolute idx ->
          fs {functions = M.insert fId (Function Nothing (Just idx) 0) fns}
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

setCallSites :: FunctionId -> FunctionState -> Int -> Maybe FunctionState
setCallSites fId fs@FunctionState {functions} count =
  case M.lookup fId functions of
    Just (Function {..}) ->
      Just $
        fs
          { functions =
              M.insert fId (Function {callSites = count, ..}) functions
          }
    Nothing -> Nothing

isRegistered :: FunctionId -> FunctionState -> Bool
isRegistered fId FunctionState {functions} = M.member fId functions

getCallerFunctionId :: (HasCallStack) => Maybe FunctionId
getCallerFunctionId =
  let s = getCallStack callStack
   in case s of
        (_, loc) : (funName, _) : _ ->
          Just
            ( Standard
                (srcLocModule loc)
                (srcLocStartLine loc)
                loc.srcLocStartCol
                funName
            )
        _ -> Nothing

getCallerConstantId :: (HasCallStack) => Maybe FunctionId
getCallerConstantId = convert <$> getCallerFunctionId
  where
    convert :: FunctionId -> FunctionId
    convert (Standard moduleName line col funName) =
      Constant moduleName line col funName
    convert _ = error ""

getCallerRtConstantId :: (HasCallStack) => Maybe FunctionId
getCallerRtConstantId = convert <$> getCallerFunctionId
  where
    convert :: FunctionId -> FunctionId
    convert (Standard moduleName line col funName) =
      RuntimeConstant moduleName line col funName
    convert _ = error ""

getCallerLambdaId :: (HasCallStack) => Maybe FunctionId
getCallerLambdaId = convert <$> getCallerFunctionId
  where
    convert :: FunctionId -> FunctionId
    convert (Standard moduleName line col funName) =
      Lambda moduleName line col funName
    convert _ = error ""

functionsSortedBySites :: FunctionTable -> [(FunctionId, Function)]
functionsSortedBySites =
  M.toList >>> sortBy (flip compare `on` ((.callSites) . snd))
