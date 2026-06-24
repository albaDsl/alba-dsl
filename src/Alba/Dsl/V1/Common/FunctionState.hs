-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionState
  ( FunctionState,
    Function (..),
    startState,
    registerFunction,
    addFunctionBody,
    getFunctionBody,
    addCallSite,
    setCallSites,
    isRegistered,
    mapFunctions,
    functionTableMap,
    getCallerFunctionId,
    getCallerConstantId,
    getCallerRtConstantId,
    getCallerQuotationId,
  )
where

import Alba.Dsl.V1.Common.InsertionOrderMap (InsertionOrderMap)
import Alba.Dsl.V1.Common.InsertionOrderMap qualified as IM
import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId (..))
import Control.Arrow ((>>>))
import Control.Monad.State (State, runState)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map qualified as M
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Prelude hiding (lookup)

newtype FunctionState = FunctionState {ft :: FunctionTable}

data Function = Function
  { code :: Maybe CodeL3,
    index :: Maybe Int,
    callSites :: Maybe Int
  }
  deriving (Show)

type FunctionTable = InsertionOrderMap FunctionId Function

startState :: FunctionState
startState = FunctionState {ft = IM.empty}

registerFunction :: FunctionId -> FunctionState -> Maybe FunctionState
registerFunction fId fs@FunctionState {ft} =
  if not (IM.member fId ft)
    then Just $
      case fId of
        Standard {} -> fs {ft = IM.insert fId (fn (Just 1)) ft}
        Constant {} -> fs {ft = IM.insert fId (fn (Just 1)) ft}
        RuntimeConstant {} -> fs {ft = IM.insert fId (fn (Just 1)) ft}
        Quotation {} -> fs {ft = IM.insert fId (fn Nothing) ft}
        Named _ -> fs {ft = IM.insert fId (fn (Just 0)) ft}
        Absolute idx ->
          fs {ft = IM.insert fId (Function Nothing (Just idx) (Just 0)) ft}
    else Nothing
  where
    fn x = Function Nothing Nothing x

addFunctionBody :: FunctionId -> CodeL3 -> FunctionState -> Maybe FunctionState
addFunctionBody fId code fs@FunctionState {ft} =
  maybe
    Nothing
    (\ft' -> Just fs {ft = ft'})
    (IM.update fId (\fn -> fn {code = Just code}) ft)

getFunctionBody :: FunctionId -> FunctionState -> Maybe CodeL3
getFunctionBody fId FunctionState {ft} =
  maybe Nothing (\fn -> fn.code) (IM.lookup fId ft)

addCallSite :: FunctionId -> FunctionState -> Maybe FunctionState
addCallSite fId fs@FunctionState {ft} =
  maybe
    Nothing
    (\ft' -> Just fs {ft = ft'})
    (IM.update fId (\fn -> fn {callSites = inc fn.callSites}) ft)
  where
    inc :: Maybe Int -> Maybe Int
    inc Nothing = Nothing
    inc (Just n) = Just (succ n)

setCallSites :: FunctionId -> FunctionState -> Int -> Maybe FunctionState
setCallSites fId fs@FunctionState {ft} count =
  maybe
    Nothing
    (\ft' -> Just fs {ft = ft'})
    (IM.update fId (\fn -> fn {callSites = Just count}) ft)

isRegistered :: FunctionId -> FunctionState -> Bool
isRegistered fId fs = IM.member fId fs.ft

functionTableMap :: FunctionState -> M.Map FunctionId Function
functionTableMap fs = IM.toMap fs.ft

mapFunctions ::
  ((FunctionId, Function) -> State Int (FunctionId, Function)) ->
  FunctionState ->
  FunctionState
mapFunctions convert fs =
  let res = runState (mapM convert functionsSortedBySitesAndInsertionOrder) 0
   in fs {ft = IM.fromList (fst res)}
  where
    functionsSortedBySitesAndInsertionOrder :: [(FunctionId, Function)]
    functionsSortedBySitesAndInsertionOrder =
      (IM.toList >>> sortBy (flip compare `on` ((.callSites) . snd))) fs.ft

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

getCallerQuotationId :: (HasCallStack) => Maybe FunctionId
getCallerQuotationId = convert <$> getCallerFunctionId
  where
    convert :: FunctionId -> FunctionId
    convert (Standard moduleName line col funName) =
      Quotation moduleName line col funName
    convert _ = error ""
