-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    FunctionTable,
    Function (..),
    startState,
    registerFunction,
    addFunctionBody,
    getFunctionBody,
    addCallSite,
    setCallSites,
    isRegistered,
    getCallerFunctionId,
    getCallerConstantId,
    getCallerRtConstantId,
    getCallerLambdaId,
    getNumRtConstants,
    mapFunctions,
    ftLookup,
    ftMapping,
  )
where

import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId (..), isRtConstant)
import Control.Arrow ((>>>))
import Control.Monad.State (State, runState)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map qualified as M
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)

newtype FunctionState = FunctionState
  { functionTable :: FunctionTable
  }
  deriving (Show)

data Function = Function
  { code :: Maybe CodeL3,
    index :: Maybe Int,
    callSites :: Maybe Int
  }
  deriving (Show)

data FunctionTable = FunctionTable
  { mapping :: M.Map FunctionId Function,
    order :: [FunctionId]
  }
  deriving (Show)

startState :: FunctionState
startState = FunctionState {functionTable = FunctionTable M.empty []}

registerFunction :: FunctionId -> FunctionState -> Maybe FunctionState
registerFunction fId fs@FunctionState {functionTable = ft} =
  if not (ftMember fId ft)
    then Just $
      case fId of
        Standard {} ->
          fs {functionTable = ftAdd fId (Function Nothing Nothing (Just 1)) ft}
        Constant {} ->
          fs {functionTable = ftAdd fId (Function Nothing Nothing (Just 1)) ft}
        RuntimeConstant {} ->
          fs {functionTable = ftAdd fId (Function Nothing Nothing (Just 1)) ft}
        Lambda {} ->
          fs {functionTable = ftAdd fId (Function Nothing Nothing Nothing) ft}
        Named _ ->
          fs {functionTable = ftAdd fId (Function Nothing Nothing (Just 0)) ft}
        Absolute idx ->
          fs
            { functionTable =
                ftAdd fId (Function Nothing (Just idx) (Just 0)) ft
            }
    else Nothing
  where

addFunctionBody :: FunctionId -> CodeL3 -> FunctionState -> Maybe FunctionState
addFunctionBody fId code fs@FunctionState {functionTable} =
  maybe
    Nothing
    (\ft' -> Just fs {functionTable = ft'})
    (ftUpdate fId (\fn -> fn {code = Just code}) functionTable)

getFunctionBody :: FunctionId -> FunctionState -> Maybe CodeL3
getFunctionBody fId FunctionState {functionTable} =
  maybe Nothing (\fn -> fn.code) (ftLookup fId functionTable)

addCallSite :: FunctionId -> FunctionState -> Maybe FunctionState
addCallSite fId fs@FunctionState {functionTable} =
  maybe
    Nothing
    (\ft' -> Just fs {functionTable = ft'})
    (ftUpdate fId (\fn -> fn {callSites = inc fn.callSites}) functionTable)
  where
    inc :: Maybe Int -> Maybe Int
    inc Nothing = Nothing
    inc (Just n) = Just (succ n)

setCallSites :: FunctionId -> FunctionState -> Int -> Maybe FunctionState
setCallSites fId fs@FunctionState {functionTable} count =
  maybe
    Nothing
    (\ft' -> Just fs {functionTable = ft'})
    (ftUpdate fId (\fn -> fn {callSites = Just count}) functionTable)

isRegistered :: FunctionId -> FunctionState -> Bool
isRegistered fId FunctionState {functionTable} = ftMember fId functionTable

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

getNumRtConstants :: FunctionTable -> Int
getNumRtConstants ft =
  M.size $ M.filterWithKey (\k _ -> isRtConstant k) ft.mapping

mapFunctions ::
  FunctionTable ->
  ((FunctionId, Function) -> State Int (FunctionId, Function)) ->
  FunctionTable
mapFunctions ft convert =
  let (res, _) =
        runState (mapM convert (functionsSortedBySitesAndInsertionOrder ft)) 0
   in ft {mapping = M.fromList res}

functionsSortedBySitesAndInsertionOrder ::
  FunctionTable -> [(FunctionId, Function)]
functionsSortedBySitesAndInsertionOrder ft =
  ( ftToList
      >>> sortByKeyInsertion ft.order
      >>> sortBy (flip compare `on` ((.callSites) . snd))
  )
    ft
  where
    sortByKeyInsertion :: (Ord k) => [k] -> [(k, v)] -> [(k, v)]
    sortByKeyInsertion order xs =
      sortBy (comparing posMap) xs
      where
        posMap = M.fromList $ zip order ([0 ..] :: [Int])
        comparing m (k1, _) (k2, _) = compare (M.lookup k1 m) (M.lookup k2 m)

-- ## Operations on FunctionTable.
ftMember :: FunctionId -> FunctionTable -> Bool
ftMember fId FunctionTable {mapping} = M.member fId mapping

ftAdd :: FunctionId -> Function -> FunctionTable -> FunctionTable
ftAdd fId fn FunctionTable {..} =
  FunctionTable {mapping = M.insert fId fn mapping, order = fId : order}

ftUpdate ::
  FunctionId -> (Function -> Function) -> FunctionTable -> Maybe FunctionTable
ftUpdate fId update ft =
  case M.lookup fId ft.mapping of
    Just fn -> Just $ ft {mapping = M.insert fId (update fn) ft.mapping}
    Nothing -> Nothing

ftLookup :: FunctionId -> FunctionTable -> Maybe Function
ftLookup fId ft =
  case M.lookup fId ft.mapping of
    Just fn -> Just $ fn
    Nothing -> Nothing

ftToList :: FunctionTable -> [(FunctionId, Function)]
ftToList ft = M.toList ft.mapping

ftMapping :: FunctionTable -> (M.Map FunctionId Function)
ftMapping ft = ft.mapping
