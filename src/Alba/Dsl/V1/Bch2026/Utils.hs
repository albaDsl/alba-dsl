-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Utils where

import Alba.Dsl.V1.Common (FnA)
import Alba.Dsl.V1.Common.CompilerUtils (aop')
import Alba.Dsl.V1.Common.FunctionState
  ( FunctionState,
    addCallSite,
    addFunctionBody,
    isRegistered,
    registerFunction,
  )
import Alba.Dsl.V1.Common.FunctionTable (Function (..), FunctionTable (..))
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (..),
    OpcodeL3 (..),
    VmFunctionId,
  )
import Alba.Dsl.V1.Common.Stack (S (..))
import Alba.Misc.Utils (canNotHappen)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack)

register :: FnA s alt s' alt' -> FunctionId -> FunctionState -> FunctionState
register prog fId fs =
  if not (isRegistered fId fs)
    then
      let fs' = fromMaybe canNotHappen (registerFunction fId fs)
          (c, fs'') = pass1 S.empty fs' prog
       in fromMaybe canNotHappen (addFunctionBody fId c fs'')
    else fromMaybe canNotHappen (addCallSite fId fs)
  where

pass1 ::
  forall s s' alt alt'.
  CodeL3 ->
  FunctionState ->
  (S s alt -> S s' alt') ->
  (CodeL3, FunctionState)
pass1 code fs prog = let S c fs' = prog (S code fs) in (c, fs')

registerQuot ::
  (HasCallStack) =>
  FunctionId ->
  FnA s1 alt1 s1' alt1' ->
  FnA s2 alt2 s2' alt2'
registerQuot fId prog st =
  (aop' (FunctionIndexRef {fId})) st {fs = register prog fId st.fs}

regErr :: a
regErr =
  error
    ( "Make sure the HasCallStack constraint is applied "
        <> "on the albaDsl macro that is defining a function/quotation."
    )

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
