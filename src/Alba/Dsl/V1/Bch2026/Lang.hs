-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang
  ( fn,
    constant,
    runtimeConstant,
    lambda0,
    lambda1,
    lambda2,
    lambda3,
    lambda4,
    invoke0,
    invoke1,
    invoke2,
    invoke3,
    invoke4,
    lambda,
    invoke,
    progCode,
    emptyProg,
    runEnv,
  )
where

import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (Env, TCode, TLambda, TLambdaUntyped)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aop', aops, aops')
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    addCallSite,
    addFunctionBody,
    getCallerConstantId,
    getCallerFunctionId,
    getCallerLambdaId,
    getCallerRtConstantId,
    getFunctionBody,
    isRegistered,
    registerFunction,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId, OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack (Fn, FnA, S (S))
import Alba.Dsl.V1.Common.TypeFamilies (Append)
import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Text.Printf (printf)

fn :: (HasCallStack) => FnA s alt s' alt' -> FnA s alt s' alt'
fn prog (S c fs) =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerFunctionId)
      fs' = register prog fId fs
   in opInvoke prog (S (aop' c (FunctionIndexRef {fId})) fs')

regErr :: a
regErr =
  error
    ( "Make sure the HasCallStack constraint is applied "
        <> "on the albaDsl macro that is defining a function/lambda."
    )

register :: FnA s alt s' alt' -> FunctionId -> FunctionState -> FunctionState
register prog fId fs =
  if not (isRegistered fId fs)
    then
      let fs' = fromMaybe canNotHappen (registerFunction fId fs)
          (c, fs'') = pass1 S.empty fs' prog
       in fromMaybe canNotHappen (addFunctionBody fId c fs'')
    else case getFunctionBody fId fs of
      Just c ->
        let (c', _) = pass1 S.empty fs prog
         in if c == c'
              then fromMaybe canNotHappen (addCallSite fId fs)
              else error (printf "%s: code for body not constant." (show fId))
      Nothing ->
        fromMaybe canNotHappen (addCallSite fId fs)

pass1 ::
  forall s s' alt alt'.
  CodeL3 ->
  FunctionState ->
  (S s alt -> S s' alt') ->
  (CodeL3, FunctionState)
pass1 code fs prog = let S c fs' = prog (S code fs) in (c, fs')

constant :: (HasCallStack) => Fn s (s > a) -> Fn s (s > a)
constant prog (S c fs) =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerConstantId)
      fs' = register prog fId fs
   in opInvoke prog (S (aop' c (FunctionIndexRef {fId})) fs')

-- Runtime constants have a size limit of maxScriptElementSize - 4 (as of
-- writing, 9996 bytes). See 'toPushOp'.
runtimeConstant :: (HasCallStack) => Fn s (s > a) -> Fn s (s > a)
runtimeConstant prog (S c fs) =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerRtConstantId)
      fs' = register prog fId fs
   in opInvoke prog (S (aop' c (FunctionIndexRef {fId})) fs')

lambda0 ::
  (HasCallStack, StackEntry r1) =>
  Fn s (s > r1) ->
  Fn s' (s' > TLambda '[] '[r1])
lambda0 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda1 ::
  (HasCallStack, StackEntry t1, StackEntry r1) =>
  Fn (s > t1) (s > r1) ->
  Fn s' (s' > TLambda '[t1] '[r1])
lambda1 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda2 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry r1) =>
  Fn (s > t1 > t2) (s > r1) ->
  Fn s' (s' > TLambda '[t1, t2] '[r1])
lambda2 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda3 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Fn (s > t1 > t2 > t3) (s > r1) ->
  Fn s' (s' > TLambda '[t1, t2, t3] '[r1])
lambda3 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda4 ::
  ( HasCallStack,
    StackEntry t1,
    StackEntry t2,
    StackEntry t3,
    StackEntry t4,
    StackEntry r1
  ) =>
  Fn (s > t1 > t2 > t3 > t4) (s > r1) ->
  Fn s' (s' > TLambda '[t1, t2, t3, t4] '[r1])
lambda4 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda :: (HasCallStack) => FnA s alt s' alt' -> Fn s'' (s'' > TLambdaUntyped)
lambda prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

registerLambda ::
  (HasCallStack) =>
  FunctionId ->
  FnA s1 alt1 s1' alt1' ->
  FnA s2 alt2 s2' alt2'
registerLambda fId prog (S c fs) =
  let fs' = register prog fId fs
   in S (aop' c (FunctionIndexRef {fId})) fs'

invoke0 :: Fn (s > TLambda '[] ret) (Append s ret)
invoke0 (S c fs) = S (aop c OP_INVOKE) fs

invoke1 :: Fn (s > t1 > TLambda '[t1] ret) (Append s ret)
invoke1 (S c fs) = S (aop c OP_INVOKE) fs

invoke2 :: Fn (s > t1 > t2 > TLambda '[t1, t2] ret) (Append s ret)
invoke2 (S c fs) = S (aop c OP_INVOKE) fs

invoke3 :: Fn (s > t1 > t2 > t3 > TLambda '[t1, t2, t3] ret) (Append s ret)
invoke3 (S c fs) = S (aop c OP_INVOKE) fs

invoke4 ::
  Fn
    (s > t1 > t2 > t3 > t4 > TLambda '[t1, t2, t3, t4] ret)
    (Append s ret)
invoke4 (S c fs) = S (aop c OP_INVOKE) fs

invoke :: FnA s alt s' alt' -> FnA (s > TLambdaUntyped) alt s' alt'
invoke _prog (S c fs) = S (aop c OP_INVOKE) fs

progCode ::
  forall s s' s'' alt' alt''.
  (FnA s' alt' s'' alt'') ->
  (Fn s (s > TCode))
progCode prog (S c fs) =
  let (c', fs') = pass1 S.empty fs prog
   in S (aop' c (FunctionBody c')) fs'

emptyProg :: Fn s s
emptyProg (S c fs) = S c fs

runEnv :: (Env s s') -> FnA s alt s' alt'
runEnv prog (S c fs) =
  let (S c' fs') = prog (S (aops' c [RuntimeState, Opcode OP_TOALTSTACK]) fs)
   in S (aops c' [OP_FROMALTSTACK, OP_DROP]) fs'
