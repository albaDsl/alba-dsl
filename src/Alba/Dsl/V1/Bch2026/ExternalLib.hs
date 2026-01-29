-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.ExternalLib (LibData (..), invokeExt) where

import Alba.Dsl.V1.Bch2025 (Bytes, FNA, FunctionTable, bytes, (#))
import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Common.FunctionStateResolved
  ( FunctionState (..),
    getVmFunctionId,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId, vmFunctionIdToByteString)
import Data.Maybe (fromMaybe)
import Prelude hiding (drop)

data LibData = LibData
  { code :: Bytes,
    size :: Int,
    hash :: Bytes,
    deployCode :: Bytes,
    deploySize :: Int,
    functionTable :: FunctionTable
  }
  deriving (Show)

invokeExt :: LibData -> FunctionId -> FNA s alt s' alt'
invokeExt lib fId = bytes ref # opInvoke prog
  where
    prog :: FNA s alt s' alt'
    prog = undefined

    ref :: Bytes
    ref = fromMaybe err $ do
      vmFId <- getVmFunctionId fId (FunctionState lib.functionTable)
      pure $ vmFunctionIdToByteString vmFId

    err = error ("invokeExt: can't find function: " <> show fId)
