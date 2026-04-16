-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.ExternalLib (LibData (..), invokeExt) where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    Fn,
    FnA,
    FunctionTable,
    TBytes,
    bytes,
    cast,
    (∘),
  )
import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (TFunctionId)
import Alba.Dsl.V1.Common.FunctionStateResolved (Function (..))
import Alba.Dsl.V1.Common.OpcodeL3
  ( FunctionId (..),
    VmFunctionId,
    vmFunctionIdToByteString,
  )
import Alba.Dsl.V1.Common.Stack (Stack (..))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
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
      let res = mapMaybe f (M.assocs lib.functionTable)
       in case res of
            [vmFId] -> vmFunctionIdToByteString vmFId
            _ -> err

    f :: (FunctionId, Function) -> Maybe VmFunctionId
    f (Standard m _l _c n, Function {vmFId})
      | m == modName && n == funName = Just vmFId
    f (Constant m _l _c n, Function {vmFId})
      | m == modName && n == funName = Just vmFId
    f (RuntimeConstant m _l _c n, Function {vmFId})
      | m == modName && n == funName = Just vmFId
    f _ = Nothing

    err =
      error
        ( "invokeExt: can't find function: "
            <> show modName
            <> ":"
            <> show funName
        )
