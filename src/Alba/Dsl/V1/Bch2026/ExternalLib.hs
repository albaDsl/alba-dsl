-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.ExternalLib (LibData (..), invokeExt) where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    Fn,
    FnA,
    FunctionTable (..),
    TBytes,
    bytes,
    cast,
    (∘),
  )
import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (TFunctionId)
import Alba.Dsl.V1.Bch2026.Utils (lookupFunctionId)
import Alba.Dsl.V1.Common.OpcodeL3 (vmFunctionIdToByteString)
import Alba.Dsl.V1.Common.Stack (Stack (..))
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

invokeExt :: LibData -> String -> String -> FnA s alt s' alt'
invokeExt lib modName funName = bytes ref ∘ b2Fid ∘ opInvoke prog
  where
    -- We know the Bytes are a valid FunctionId so can cast it.
    b2Fid :: Fn (s :> TBytes) (s :> TFunctionId)
    b2Fid = cast

    prog :: FnA s alt s' alt'
    prog = undefined

    ref :: Bytes
    ref =
      vmFunctionIdToByteString $
        lookupFunctionId lib.functionTable modName funName
