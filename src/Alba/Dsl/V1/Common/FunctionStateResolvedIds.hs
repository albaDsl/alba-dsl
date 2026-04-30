-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.FunctionStateResolvedIds
  ( FunctionState (..),
    FunctionTable,
    Function (..),
    toResolved,
    getVmFunctionId,
    getBaseVmFunctionId,
  )
where

import Alba.Dsl.V1.Common.FunctionState qualified as FS
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (..),
    FunctionIdType (..),
    VmFunctionId,
    mkVmFunctionId,
  )
import Data.Map qualified as M
import Text.Printf (printf)

newtype FunctionState = FunctionState
  { functionTable :: FunctionTable
  }
  deriving (Show)

data Function = Function
  { code :: Maybe CodeL3,
    index :: Int,
    vmFId :: VmFunctionId,
    callSites :: Maybe Int
  }
  deriving (Eq, Show)

type FunctionTable = M.Map FunctionId Function

toResolved :: FunctionIdType -> FS.FunctionState -> FunctionState
toResolved fIdType fs =
  FunctionState {functionTable = M.map convert (FS.functionTableMap fs)}
  where
    convert :: FS.Function -> Function
    convert f@FS.Function {..} = do
      case index of
        Just idx ->
          Function {index = idx, vmFId = mkVmFunctionId fIdType idx, ..}
        Nothing ->
          error (printf "toResolved: Function has undefined index: %s" (show f))

getVmFunctionId :: FunctionId -> FunctionState -> Maybe VmFunctionId
getVmFunctionId fId FunctionState {functionTable} =
  case M.lookup fId functionTable of
    Just (Function {vmFId}) -> pure vmFId
    Nothing -> Nothing

-- Base for runtime assigned function IDs.
getBaseVmFunctionId :: FunctionIdType -> FunctionState -> VmFunctionId
getBaseVmFunctionId Local FunctionState {functionTable} =
  if M.null functionTable
    then mkVmFunctionId Local 0
    else
      let maxIdx = maximum ((.index) <$> functionTable)
       in mkVmFunctionId Local (succ maxIdx)
getBaseVmFunctionId _ _ =
  error "RuntimeState can't be initialized from libraries."
