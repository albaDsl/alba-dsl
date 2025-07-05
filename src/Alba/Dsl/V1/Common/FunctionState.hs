-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    Function (..),
    FunctionId,
    startState,
    addFunction,
    addFunctionBody,
    addLambda,
    addCallSite,
    isRegistered,
    getSlot,
    getCallerFunctionId,
    functionsSorted,
    functionsSummary,
  )
where

import Alba.Vm.Common.OpcodeL2 (CodeL2)
import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Text.Printf (printf)

data FunctionState = FunctionState
  { lambdaIdx :: Int,
    functions :: M.Map FunctionId Function
  }
  deriving (Show)

type FunctionId = (ModuleName, LineNumber, FunctionName)

type ModuleName = String

type LineNumber = Int

type FunctionName = String

data Function = Function
  { code :: Maybe CodeL2,
    slot :: Maybe Int,
    callSites :: Int
  }
  deriving (Show)

startState :: FunctionState
startState = FunctionState {lambdaIdx = 0, functions = M.empty}

addFunction :: FunctionId -> FunctionState -> Maybe FunctionState
addFunction fId fs@FunctionState {functions} =
  if not (M.member fId functions)
    then
      Just $
        fs
          { functions =
              M.insert fId (Function Nothing Nothing 1) functions
          }
    else Nothing

addFunctionBody :: FunctionId -> CodeL2 -> FunctionState -> Maybe FunctionState
addFunctionBody fId code fs@FunctionState {functions} =
  case M.lookup fId functions of
    Just f ->
      Just $
        fs
          { functions = M.insert fId (f {code = Just code}) functions
          }
    Nothing -> Nothing

addLambda :: FunctionId -> FunctionState -> Maybe FunctionState
addLambda fId fs@FunctionState {..} =
  if not (M.member fId functions)
    then
      Just $
        fs
          { lambdaIdx = succ lambdaIdx,
            functions = M.insert fId (Function Nothing Nothing 0) functions
          }
    else Nothing

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
          Just (srcLocModule loc, srcLocStartLine loc, fun)
        _ -> Nothing

functionsSorted :: M.Map FunctionId Function -> [(FunctionId, Function)]
functionsSorted = M.toList >>> sortBy (flip compare `on` ((.callSites) . snd))

functionsSummary :: FunctionState -> String
functionsSummary FunctionState {functions} =
  let hline = replicate tableWidth '-' <> "\n"
   in line "Module" "Line" "Function" "Ops" "Slot" "Sites"
        <> hline
        <> foldr
          ( \((moduleName, lineNumber, functionName), Function {..}) a ->
              line
                (trunc widthModule moduleName)
                (trunc widthLine (show lineNumber))
                (trunc widthFunction functionName)
                (trunc widthOps (maybe "-" (show . S.length) code))
                (trunc widthSlot (maybe "?" show slot))
                (trunc widthSites (show callSites))
                <> a
          )
          ""
          (functionsSorted functions)
  where
    widthModule = 40 :: Int
    widthLine = 5 :: Int
    widthFunction = 25 :: Int
    widthOps = 5 :: Int
    widthSlot = 5 :: Int
    widthSites = 5 :: Int

    tableWidth =
      widthModule
        + widthLine
        + widthFunction
        + widthOps
        + widthSlot
        + widthSites
        + 5

    formattingStr = "%-*s %-*s %-*s %-*s %-*s %-*s\n"

    line :: String -> String -> String -> String -> String -> String -> String
    line modStr lineStr funStr bytesStr slotStr sitesStr =
      printf
        formattingStr
        widthModule
        modStr
        widthLine
        lineStr
        widthFunction
        funStr
        widthOps
        bytesStr
        widthSlot
        slotStr
        widthSites
        sitesStr

    trunc n str =
      if length str > n
        then take (pred n) str <> "$"
        else str
