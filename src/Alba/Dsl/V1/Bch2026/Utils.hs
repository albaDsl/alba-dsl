-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Utils where

import Alba.Dsl.V1.Common.FunctionTable (Function (..), FunctionTable (..))
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (..), VmFunctionId)
import Data.Maybe (mapMaybe)

lookupFunctionId :: FunctionTable -> String -> String -> VmFunctionId
lookupFunctionId (FunctionTable ls) modName funName =
  case mapMaybe f ls of
    [vmFId] -> vmFId
    _ -> err
  where
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
        ( "lookupFunctionId: can't find function: "
            <> show modName
            <> ":"
            <> show funName
        )
